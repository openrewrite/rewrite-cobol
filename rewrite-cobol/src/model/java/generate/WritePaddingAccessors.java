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
import org.openrewrite.java.tree.J;
import org.openrewrite.java.tree.JavaType;
import org.openrewrite.java.tree.Statement;
import org.openrewrite.java.tree.TypeUtils;

import java.util.StringJoiner;

/**
 * TODO Unable to add accessors in the first phase due to some bug in JavaTemplate.
 */
@RequiredArgsConstructor
public class WritePaddingAccessors extends Recipe {
    @Override
    public String getDisplayName() {
        return "Write accessors for padded parts of the model";
    }

    @Override
    public String getDescription() {
        return "Write accessors for padded parts of the model.";
    }

    JavaParser.Builder<?, ?> parser = JavaParser.fromJavaVersion().classpath(JavaParser.runtimeClasspath());

    @RequiredArgsConstructor
    class WritePaddingAccessorsVisitor extends JavaIsoVisitor<ExecutionContext> {
        final J.ClassDeclaration modelClassDeclaration;

        /**
         * The accessors in the Padding class that return the padding wrapped element.
         */
        final JavaTemplate paddedGetterWither = JavaTemplate.builder("" +
                "#{}" +
                "public Cobol#{}<#{}> get#{}() {" +
                "    return t.#{};" +
                "}" +
                "public #{} with#{}(#{}Cobol#{}<#{}> #{}) {" +
                "    return t.#{} == #{} ? t : new #{}(#{});" +
                "}").contextSensitive().javaParser(parser).build();

        @Override
        public J.ClassDeclaration visitClassDeclaration(J.ClassDeclaration classDecl, ExecutionContext ctx) {
            J.ClassDeclaration c = classDecl;

            if (c.getSimpleName().equals("Padding")) {
                for (Statement statement : modelClassDeclaration.getBody().getStatements()) {
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

                        if (fqn != null && elementType != null) {
                            switch (fqn.getClassName()) {
                                case "CobolContainer":
                                    c = writePaddedGetterWithers(c, varDec, elementType, "Container");
                                    break;
                                case "CobolLeftPadded":
                                    c = writePaddedGetterWithers(c, varDec, elementType, "LeftPadded");
                                    break;
                                case "CobolRightPadded":
                                    c = writePaddedGetterWithers(c, varDec, elementType, "RightPadded");
                                    break;
                            }
                        }
                    }
                }

                return c;
            }

            return super.visitClassDeclaration(classDecl, ctx);
        }

        private J.ClassDeclaration writePaddedGetterWithers(J.ClassDeclaration c, J.VariableDeclarations varDec, JavaType.FullyQualified elementType,
                                                            String leftOrRight) {
            boolean nullable = !FindAnnotations.find(varDec, "@org.openrewrite.internal.lang.Nullable").isEmpty();
            String name = varDec.getVariables().get(0).getSimpleName();
            String capitalizedName = name.substring(0, 1).toUpperCase() + name.substring(1);
            String elementTypeName = elementType.getClassName();
            String modelTypeName = modelClassDeclaration.getSimpleName();

            StringJoiner newModelArguments = new StringJoiner(", ");
            for (Statement paddingStatement : modelClassDeclaration.getBody().getStatements()) {
                if (paddingStatement instanceof J.VariableDeclarations) {
                    newModelArguments.add(paddingStatement == varDec ? name : "t." + ((J.VariableDeclarations) paddingStatement).getVariables()
                            .get(0).getSimpleName());
                }
            }

            c = paddedGetterWither.apply(updateCursor(c), c.getBody().getCoordinates().lastStatement(),
                    nullable ? "@Nullable " : "", leftOrRight, elementTypeName, capitalizedName,
                    name, modelTypeName, capitalizedName,
                    nullable ? "@Nullable " : "", leftOrRight,
                    elementTypeName, name, name, name, modelTypeName, newModelArguments);

            return c;
        }

        boolean isPadded(Statement statement) {
            JavaType.FullyQualified type = TypeUtils.asFullyQualified(((J.VariableDeclarations) statement).getType());
            assert type != null;
            return type.getClassName().contains("Padded") || type.getClassName().equals("CobolContainer");
        }
    }

    @Override
    public JavaVisitor<ExecutionContext> getVisitor() {
        return new JavaIsoVisitor<ExecutionContext>() {
            @Override
            public J.Block visitBlock(J.Block block, ExecutionContext ctx) {
                Object parent = getCursor().getParentOrThrow().getValue();
                if (!(parent instanceof J.ClassDeclaration) || !((J.ClassDeclaration) parent).getSimpleName().equals("Cobol")) {
                    return block;
                }

                J.Block b = block;

                b = b.withStatements(ListUtils.map(b.getStatements(),
                        mc -> mc instanceof J.ClassDeclaration ?
                                (Statement) new WritePaddingAccessorsVisitor((J.ClassDeclaration) mc)
                                        .visitNonNull(mc, ctx, getCursor().getParentOrThrow()) :
                                mc)
                );

                return b;
            }
        };
    }
}
