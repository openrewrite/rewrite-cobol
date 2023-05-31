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
import org.openrewrite.java.tree.J;
import org.openrewrite.java.tree.JavaType;
import org.openrewrite.java.tree.Statement;
import org.openrewrite.java.tree.TypeUtils;

import java.util.List;
import java.util.StringJoiner;

import static java.util.Objects.requireNonNull;

@RequiredArgsConstructor
public class WritePrinter extends Recipe {
    final List<J.ClassDeclaration> modelClasses;

    @Override
    public String getDisplayName() {
        return "Write the boilerplate for `CobolPrinter`";
    }

    @Override
    public String getDescription() {
        return "Every print method starts with `visitSpace` then `visitMarkers`. " +
                "Every model element is visited. An engineer must fill in the places " +
                "where keywords are grammatically required.";
    }

    JavaParser.Builder<?, ?> parser = JavaParser.fromJavaVersion().classpath(JavaParser.runtimeClasspath());

    @Override
    public JavaVisitor<ExecutionContext> getVisitor() {
        return new JavaIsoVisitor<ExecutionContext>() {
            @Override
            public J.ClassDeclaration visitClassDeclaration(J.ClassDeclaration classDecl, ExecutionContext ctx) {
                J.ClassDeclaration c = classDecl;

                for (J.ClassDeclaration modelClass : missingVisitorMethods(c)) {
                    String modelTypeName = modelClass.getSimpleName();
                    String paramName = modelTypeName.substring(0, 1).toLowerCase() + modelTypeName.substring(1);

                    StringJoiner fields = new StringJoiner("\n    ");
                    for (Statement statement : modelClass.getBody().getStatements()) {
                        if (statement instanceof J.VariableDeclarations) {
                            J.VariableDeclarations varDec = (J.VariableDeclarations) statement;
                            J.VariableDeclarations.NamedVariable namedVariable = varDec.getVariables().get(0);
                            String name = namedVariable.getSimpleName();
                            String elemTypeName = requireNonNull(varDec.getTypeExpression()).printTrimmed(getCursor());
                            String capitalizedName = name.substring(0, 1).toUpperCase() + name.substring(1);

                            JavaType.FullyQualified elemType = requireNonNull(TypeUtils.asFullyQualified(varDec.getType()));
                            switch (elemType.getClassName()) {
                                case "CobolLeftPadded":
                                    fields.add("visitLeftPadded(\"\"," + paramName + ".getPadding().get" + capitalizedName + "(), p);");
                                    break;
                                case "CobolRightPadded":
                                    fields.add("visitRightPadded(" + paramName + ".getPadding().get" + capitalizedName + "(), \"\", p);");
                                    break;
                                case "CobolContainer":
                                    fields.add("visitContainer(\"\", " + paramName + ".getPadding().get" + capitalizedName + "(), \"\", \"\", p);");
                                    break;
                                case "List":
                                    String loopVar = paramName.substring(0, 1);
                                    if (loopVar.equals("p")) {
                                        loopVar = "pp";
                                    }
                                    String typeParam = ((J.Identifier) ((J.ParameterizedType) varDec.getTypeExpression()).getTypeParameters().get(0)).getSimpleName();
                                    fields.add("for(Cobol." + typeParam + " " + loopVar + " : " + paramName + ".get" + capitalizedName + "()) {\n" +
                                            "    // TODO print each element\n" +
                                            "}");
                                    break;
                                case "String":
                                    fields.add("p.append(" + paramName + ".get" + capitalizedName + "());");
                                    break;
                                case "Integer":
                                    fields.add("p.append(" + paramName + ".get" + capitalizedName + "().toString());");
                                    break;
                                default:
                                    if (elemType.getClassName().startsWith("Cobol")) {
                                        fields.add("visit(" + paramName + ".get" + capitalizedName + "(), p);");
                                    }
                            }
                        }
                    }

                    StringBuilder template = new StringBuilder();

                    JavaTemplate visitMethod = JavaTemplate.builder("" +
                                    "public Cobol visit#{}(Cobol.#{} #{}, PrintOutputCapture<P> p) {" +
                                    "    visitSpace(#{}.getPrefix(), p);" +
                                    "    visitMarkers(#{}.getMarkers(), p);" +
                                    "    #{}" +
                                    "    return #{};" +
                                    "}"
                            )
                            .context(this::getCursor)
                            .javaParser(parser)
//                            .doAfterVariableSubstitution(System.out::println)
                            .doBeforeParseTemplate(template::append)
                            .build();

                    try {
                        c = c.withTemplate(visitMethod, getCursor(), c.getBody().getCoordinates().lastStatement(),
                                modelTypeName, modelTypeName, paramName,
                                paramName,
                                paramName,
                                fields,
                                paramName);
                    } catch(Throwable t) {
                        System.out.println(template);
                        throw t;
                    }
                }

                return c;
            }

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
        };
    }
}
