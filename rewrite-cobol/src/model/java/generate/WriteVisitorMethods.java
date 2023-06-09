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

import java.util.List;
import java.util.StringJoiner;

import static java.util.Objects.requireNonNull;

@RequiredArgsConstructor
public class WriteVisitorMethods extends Recipe {
    final List<J.ClassDeclaration> modelClasses;

    @Override
    public String getDisplayName() {
        return "Write the boilerplate for `CobolVisitor` and `CobolIsoVisitor`";
    }

    @Override
    public String getDescription() {
        return "Write the boilerplate for `CobolVisitor` and `CobolIsoVisitor`.";
    }

    @Override
    public JavaVisitor<ExecutionContext> getVisitor() {
        return new JavaVisitor<ExecutionContext>() {
            @Override
            public J visitClassDeclaration(J.ClassDeclaration classDecl, ExecutionContext ctx) {
                switch (classDecl.getSimpleName()) {
                    case "CobolVisitor":
                        return writeVisitorMethods.visitNonNull(classDecl, ctx, getCursor().getParentOrThrow());
                    case "CobolIsoVisitor":
                        return writeIsoVisitorMethods.visitNonNull(classDecl, ctx, getCursor().getParentOrThrow());
                }

                return classDecl;
            }
        };
    }

    JavaParser.Builder<?, ?> parser = JavaParser.fromJavaVersion().classpath(JavaParser.runtimeClasspath());

    private final JavaVisitor<ExecutionContext> writeVisitorMethods = new JavaIsoVisitor<ExecutionContext>() {
        final JavaTemplate visitMethod = JavaTemplate.builder("" +
                "public Cobol visit#{}(Cobol.#{} #{}, P p) {" +
                "    Cobol.#{} #{} = #{};" +
                "    #{} = #{}.withPrefix(visitSpace(#{}.getPrefix(), p));" +
                "    #{} = #{}.withMarkers(visitMarkers(#{}.getMarkers(), p));" +
                "    #{}" +
                "    return #{};" +
                "}").contextSensitive().javaParser(parser).build();

        @Override
        public J.ClassDeclaration visitClassDeclaration(J.ClassDeclaration classDecl, ExecutionContext ctx) {
            J.ClassDeclaration c = classDecl;

            for (J.ClassDeclaration modelClass : missingVisitorMethods(c)) {
                String modelTypeName = modelClass.getSimpleName();
                String paramName = modelTypeName.substring(0, 1).toLowerCase() + modelTypeName.substring(1);
                String varName = paramName.substring(0, 1);
                if (varName.equals("p")) {
                    varName = "pp";
                }

                StringJoiner fields = new StringJoiner("\n    ");
                for (Statement statement : modelClass.getBody().getStatements()) {
                    if (statement instanceof J.VariableDeclarations) {
                        J.VariableDeclarations varDec = (J.VariableDeclarations) statement;
                        boolean nullable = !FindAnnotations.find(varDec, "@org.openrewrite.internal.lang.Nullable").isEmpty();
                        String name = varDec.getVariables().get(0).getSimpleName();
                        String capitalizedName = name.substring(0, 1).toUpperCase() + name.substring(1);

                        JavaType.FullyQualified elemType = requireNonNull(TypeUtils.asFullyQualified(varDec.getType()));
                        switch (elemType.getClassName()) {
                            case "CobolLeftPadded":
                                if(nullable) {
                                    fields.add("if(" + varName + ".getPadding().get" + capitalizedName + "() != null) {");
                                }
                                fields.add(varName + " = " + varName + ".getPadding().with" + capitalizedName + "(visitLeftPadded(" +
                                        varName + ".getPadding().get" + capitalizedName + "(), p));");
                                if(nullable) {
                                    fields.add("}");
                                }
                                break;
                            case "CobolRightPadded":
                                if(nullable) {
                                    fields.add("if(" + varName + ".getPadding().get" + capitalizedName + "() != null) {");
                                }
                                fields.add(varName + " = " + varName + ".getPadding().with" + capitalizedName + "(visitRightPadded(" +
                                        varName + ".getPadding().get" + capitalizedName + "(), p));");
                                if(nullable) {
                                    fields.add("}");
                                }
                                break;
                            case "CobolContainer":
                                fields.add(varName + " = " + varName + ".getPadding().with" + capitalizedName + "(visitContainer(" + varName + ".getPadding().get" + capitalizedName + "(), p));");
                                break;
                            case "List":
                                J.ParameterizedType parameterizedType = requireNonNull((J.ParameterizedType) varDec.getTypeExpression());
                                String elemListType = requireNonNull(TypeUtils.asFullyQualified(requireNonNull(parameterizedType.getTypeParameters()).get(0).getType()))
                                        .getClassName();
                                fields.add(varName + " = " + varName + ".with" + capitalizedName + "(ListUtils.map(" +
                                        varName + ".get" + capitalizedName + "(), t -> (" + elemListType +
                                        ") visit(t, p)));");
                                break;
                            default:
                                if(elemType.getClassName().startsWith("Cobol")) {
                                    fields.add(varName + " = " + varName + ".with" + capitalizedName + "((" +
                                            elemType.getClassName() + ") visit(" + varName + ".get" + capitalizedName + "(), p));");
                                }
                        }
                    }
                }

                c = visitMethod.apply(updateCursor(c), c.getBody().getCoordinates().lastStatement(),
                        modelTypeName, modelTypeName, paramName,
                        modelTypeName, varName, paramName,
                        varName, varName, varName,
                        varName, varName, varName,
                        fields,
                        varName);
            }

            return c;
        }
    };

    private final JavaVisitor<ExecutionContext> writeIsoVisitorMethods = new JavaIsoVisitor<>() {
        final JavaTemplate isoVisitMethod = JavaTemplate.builder("" +
                                                                 "@Override " +
                                                                 "public Cobol.#{} visit#{}(Cobol.#{} #{}, P p) {" +
                                                                 "    return (Cobol.#{}) super.visit#{}(#{}, p);" +
                                                                 "}"
        ).contextSensitive().javaParser(parser).build();

        @Override
        public J.ClassDeclaration visitClassDeclaration(J.ClassDeclaration classDecl, ExecutionContext ctx) {
            J.ClassDeclaration c = classDecl;

            for (J.ClassDeclaration modelClass : missingVisitorMethods(c)) {
                String modelTypeName = modelClass.getSimpleName();
                String paramName = modelTypeName.substring(0, 1).toLowerCase() + modelTypeName.substring(1);

                c = isoVisitMethod.apply(updateCursor(c), c.getBody().getCoordinates().lastStatement(),
                        modelTypeName, modelTypeName, modelTypeName, paramName,
                        modelTypeName, modelTypeName, paramName);
            }

            return c;
        }
    };

    private List<J.ClassDeclaration> missingVisitorMethods(J.ClassDeclaration visitorClass) {
        return ListUtils.map(modelClasses, modelClass -> {
            for (Statement statement : visitorClass.getBody().getStatements()) {
                if (statement instanceof J.MethodDeclaration) {
                    J.MethodDeclaration m = (J.MethodDeclaration) statement;
                    if (m.getSimpleName().endsWith(modelClass.getSimpleName())) {
                        return null;
                    }
                }
            }
            return modelClass;
        });
    }
}
