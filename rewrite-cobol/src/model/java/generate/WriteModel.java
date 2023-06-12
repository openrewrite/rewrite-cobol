/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package generate;

import lombok.RequiredArgsConstructor;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Recipe;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.java.JavaIsoVisitor;
import org.openrewrite.java.JavaParser;
import org.openrewrite.java.JavaTemplate;
import org.openrewrite.java.JavaVisitor;
import org.openrewrite.java.search.FindAnnotations;
import org.openrewrite.java.tree.*;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.StringJoiner;

@RequiredArgsConstructor
public class WriteModel extends Recipe {
    final List<J.ClassDeclaration> modelClasses;

    @Override
    public String getDisplayName() {
        return "Write the AST model";
    }

    @Override
    public String getDescription() {
        return "Expand the model into an AST with Lombok annotations, Padding classes, etc.";
    }

    JavaParser.Builder<?, ?> parser = JavaParser.fromJavaVersion().classpath(JavaParser.runtimeClasspath());

    JavaVisitor<ExecutionContext> writeModelClass = new JavaIsoVisitor<ExecutionContext>() {
        final JavaTemplate valueModel = JavaTemplate.builder("" +
                "@Value " +
                "@EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true) " +
                "@With").contextSensitive().javaParser(parser).build();

        final JavaTemplate paddedModel = JavaTemplate.builder("" +
                "@ToString " +
                "@FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE) " +
                "@EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true) " +
                "@RequiredArgsConstructor " +
                "@AllArgsConstructor(access = AccessLevel.PRIVATE)")
                .contextSensitive().javaParser(parser).build();

        final JavaTemplate idField = JavaTemplate.builder("@EqualsAndHashCode.Include UUID id;")
                .contextSensitive().javaParser(parser).build();
        final JavaTemplate prefixField = JavaTemplate.builder("Space prefix;")
                .contextSensitive().javaParser(parser).build();
        final JavaTemplate markersField = JavaTemplate.builder("Markers markers;")
                .contextSensitive().javaParser(parser).build();
        final JavaTemplate paddingField = JavaTemplate.builder("@Nullable @NonFinal transient WeakReference<Padding> padding;")
                .contextSensitive().javaParser(parser).build();

        final JavaTemplate getPadding = JavaTemplate.builder("" +
                        "public Padding getPadding() {" +
                        "    Padding p;" +
                        "    if (this.padding == null) {" +
                        "        p = new Padding(this);" +
                        "        this.padding = new WeakReference<>(p);" +
                        "    } else {" +
                        "        p = this.padding.get();" +
                        "        if (p == null || p.t != this) {" +
                        "            p = new Padding(this);" +
                        "            this.padding = new WeakReference<>(p);" +
                        "        }" +
                        "    }" +
                        "    return p;" +
                        "}")
                .contextSensitive()
                .build();

        final JavaTemplate paddingClass = JavaTemplate.builder("" +
                        "@RequiredArgsConstructor " +
                        "public static class Padding {" +
                        "    private final #{} t;" +
                        "}")
                .contextSensitive()
                .build();

        final JavaTemplate acceptMethod = JavaTemplate.builder("" +
                "@Override public <P> Cobol acceptCobol(CobolVisitor<P> v, P p) {" +
                "  return v.visit#{}(this, p);" +
                "}").contextSensitive().javaParser(parser).build();

        /**
         * The accessors in the model class that skips the padding and return the contained element.
         */
        final JavaTemplate unwrappedPaddedGetterWither = JavaTemplate.builder("" +
                "public #{} get#{}() {" +
                "    return #{}.getElement();" +
                "}" +
                "public #{} with#{}(#{} #{}) {\n" +
                "    //noinspection ConstantConditions\n" +
                "    return getPadding().with#{}(Cobol#{}Padded.withElement(this.#{}, #{}));" +
                "}").contextSensitive().javaParser(parser).build();

        final JavaTemplate nullableUnwrappedPaddedGetterWither = JavaTemplate.builder("" +
                "@Nullable " +
                "public #{} get#{}() {" +
                "    return #{} == null ? null : #{}.getElement();" +
                "} " +
                "public #{} with#{}(@Nullable #{} #{}) {" +
                "    if (#{} == null) {" +
                "        return this.#{} == null ? this : new #{}(#{});" +
                "    }" +
                "    return getPadding().with#{}(Cobol#{}Padded.withElement(this.#{}, #{}));" +
                "}").contextSensitive().javaParser(parser).build();

        /**
         * The accessors in the model class that skips the padding and return the contained elements.
         */
        final JavaTemplate unwrappedContainerGetterWither = JavaTemplate.builder("" +
                "public List<#{}> get#{}() {" +
                "    return #{}.getElements();" +
                "}" +
                "public #{} with#{}(List<#{}> #{}) {\n" +
                "    return getPadding().with#{}(this.#{}.getPadding().withElements(CobolRightPadded.withElements(\n" +
                "        this.#{}.getPadding().getElements(), #{}));" +
                "}").contextSensitive().javaParser(parser).build();

        final JavaTemplate withGetterAnnotations = JavaTemplate.builder("@With @Getter")
                .contextSensitive().javaParser(parser).build();

        @Override
        public J.ClassDeclaration visitClassDeclaration(J.ClassDeclaration classDecl, ExecutionContext ctx) {
            J.ClassDeclaration c = classDecl;
            if(FindAnnotations.find(c, "@generate.Skip").size() > 0) {
                //noinspection ConstantConditions
                return null;
            }

            boolean padded = c.getBody().getStatements().stream().anyMatch(this::isPadded);

            c = markersField.apply(updateCursor(c), c.getBody().getCoordinates().firstStatement());
            c = prefixField.apply(updateCursor(c), c.getBody().getCoordinates().firstStatement());
            c = idField.apply(updateCursor(c), c.getBody().getCoordinates().firstStatement());
            c = acceptMethod.apply(updateCursor(c), c.getBody().getCoordinates().lastStatement(), classDecl.getSimpleName());

            for (Statement statement : c.getBody().getStatements()) {
                if (statement instanceof J.VariableDeclarations) {
                    J.VariableDeclarations varDec = (J.VariableDeclarations) statement;
                    JavaType.FullyQualified fqn = TypeUtils.asFullyQualified(varDec.getType());

                    JavaType.FullyQualified elementType = null;
                    if (varDec.getTypeExpression() instanceof J.ParameterizedType) {
                        J.ParameterizedType typeExpression = (J.ParameterizedType) varDec.getTypeExpression();
                        if (typeExpression.getTypeParameters() != null) {
                            elementType = TypeUtils.asFullyQualified(typeExpression.getTypeParameters().get(0).getType());
                        }
                    }

                    if (fqn != null) {
                        if (elementType != null) {
                            switch (fqn.getClassName()) {
                                case "CobolContainer":
                                    c = writeContainerGetterWithers(c, varDec, elementType);
                                    break;
                                case "CobolLeftPadded":
                                    c = writePaddedGetterWithers(c, varDec, elementType, "Left");
                                    break;
                                case "CobolRightPadded":
                                    c = writePaddedGetterWithers(c, varDec, elementType, "Right");
                                    break;
                            }
                        } else if (padded) {
                            c = withGetterAnnotations.apply(updateCursor(c), varDec.getCoordinates()
                                    .addAnnotation(Comparator.comparing(J.Annotation::getSimpleName)));
                        }
                    }
                }
            }

            if (padded) {
                c = paddedModel.apply(updateCursor(c), c.getCoordinates().replaceAnnotations());
                c = paddingField.apply(updateCursor(c), c.getBody().getCoordinates().firstStatement());
                c = getPadding.apply(updateCursor(c), c.getBody().getCoordinates().lastStatement());
                c = paddingClass.apply(updateCursor(c), c.getBody().getCoordinates().lastStatement(), c.getSimpleName());
            } else {
                c = valueModel.apply(updateCursor(c), c.getCoordinates().replaceAnnotations());
            }

            List<Statement> statements = c.getBody().getStatements();
            c = c.withBody(c.getBody().withStatements(ListUtils.map(statements, (i, statement) -> {
                if (statement instanceof J.VariableDeclarations && i > 0) {
                    Statement previous = statements.get(i - 1);
                    if (!((J.VariableDeclarations) statement).getAllAnnotations().isEmpty() ||
                            (previous instanceof J.VariableDeclarations) && !((J.VariableDeclarations) previous).getAllAnnotations().isEmpty()) {
                        return statement.withPrefix(Space.format("\n\n"));
                    }
                }
                return statement;
            })));

            return c;
        }

        private J.ClassDeclaration writeContainerGetterWithers(J.ClassDeclaration c, J.VariableDeclarations varDec, JavaType.FullyQualified elementType) {
            String name = varDec.getVariables().get(0).getSimpleName();
            String capitalizedName = name.substring(0, 1).toUpperCase() + name.substring(1);
            String elementTypeName = elementType.getClassName();
            String modelTypeName = c.getSimpleName();

            c = unwrappedContainerGetterWither.apply(updateCursor(c), c.getBody().getCoordinates().lastStatement(),
                    elementTypeName, capitalizedName,
                    name,
                    modelTypeName, capitalizedName, elementTypeName, name,
                    capitalizedName, name,
                    name, name);

            return c;
        }

        private J.ClassDeclaration writePaddedGetterWithers(J.ClassDeclaration c, J.VariableDeclarations varDec, JavaType.FullyQualified elementType,
                                                            String leftOrRight) {
            boolean nullable = !FindAnnotations.find(varDec, "@org.openrewrite.internal.lang.Nullable").isEmpty();
            String name = varDec.getVariables().get(0).getSimpleName();
            String capitalizedName = name.substring(0, 1).toUpperCase() + name.substring(1);
            String elementTypeName = elementType.getClassName();
            String modelTypeName = c.getSimpleName();

            if (nullable) {
                StringJoiner newModelArguments = new StringJoiner(", ");
                for (Statement statement : c.getBody().getStatements()) {
                    if (statement instanceof J.VariableDeclarations) {
                        newModelArguments.add(statement == varDec ? "null" : ((J.VariableDeclarations) statement).getVariables()
                                .get(0).getSimpleName());
                    }
                }
                c = nullableUnwrappedPaddedGetterWither.apply(updateCursor(c), c.getBody().getCoordinates().lastStatement(),
                        elementTypeName, capitalizedName, name, name, modelTypeName, capitalizedName,
                        elementTypeName, name, name, name, modelTypeName, newModelArguments.toString(),
                        capitalizedName, leftOrRight, name, name);
            } else {
                c = unwrappedPaddedGetterWither.apply(updateCursor(c), c.getBody().getCoordinates().lastStatement(),
                        elementTypeName, capitalizedName, name,
                        modelTypeName, capitalizedName, elementTypeName, name,
                        capitalizedName, leftOrRight, name, name);
            }

            return c;
        }

        boolean isPadded(Statement statement) {
            if (!(statement instanceof J.VariableDeclarations)) {
                return false;
            }
            JavaType.FullyQualified type = TypeUtils.asFullyQualified(((J.VariableDeclarations) statement).getType());
            assert type != null;
            return type.getClassName().contains("Padded") || type.getClassName().equals("CobolContainer");
        }
    };

    @Override
    public JavaVisitor<ExecutionContext> getVisitor() {
        return new JavaIsoVisitor<>() {
            @Override
            public J.Block visitBlock(J.Block block, ExecutionContext ctx) {
                Object parent = getCursor().getParentOrThrow().getValue();
                if (!(parent instanceof J.ClassDeclaration) || !((J.ClassDeclaration) parent).getSimpleName().equals("Cobol")) {
                    return block;
                }

                J.Block b = block.withStatements(ListUtils.map(block.getStatements(), s -> s instanceof J.ClassDeclaration &&
                                                                                           !(((J.ClassDeclaration) s).getSimpleName().equals("CompilationUnit")) ? null : s));
                List<Statement> statements = new ArrayList<>(b.getStatements());
                statements.addAll(ListUtils.map(modelClasses,
                        mc -> (J.ClassDeclaration) writeModelClass.visitNonNull(mc, ctx, getCursor().getParentOrThrow())));
                b = b.withStatements(statements);

                return b;
            }
        };
    }
}
