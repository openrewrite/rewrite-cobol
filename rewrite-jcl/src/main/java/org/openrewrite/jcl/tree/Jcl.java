package org.openrewrite.jcl.tree;

import lombok.*;
import lombok.experimental.FieldDefaults;
import lombok.experimental.NonFinal;
import org.openrewrite.*;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.JclVisitor;
import org.openrewrite.jcl.internal.JclPrinter;
import org.openrewrite.marker.Markers;

import java.lang.ref.WeakReference;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;

import static java.util.Objects.requireNonNull;

public interface Jcl extends Tree {

    @SuppressWarnings("unchecked")
    @Override
    default <R extends Tree, P> R accept(TreeVisitor<R, P> v, P p) {
        return (R) acceptJcl(v.adapt(JclVisitor.class), p);
    }

    @Nullable
    default <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
        return v.defaultValue(this, p);
    }

    @Override
    default <P> boolean isAcceptable(TreeVisitor<?, P> v, P p) {
        return v.isAdaptableTo(JclVisitor.class);
    }

    Space getPrefix();

    <P extends Jcl> P withPrefix(Space prefix);


    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class CompilationUnit implements Jcl, SourceFile {

        @EqualsAndHashCode.Include
        UUID id;

        Path sourcePath;

        @Nullable
        FileAttributes fileAttributes;

        Space prefix;
        Markers markers;

        @Nullable // for backwards compatibility
        @With(AccessLevel.PRIVATE)
        String charsetName;

        boolean charsetBomMarked;

        @Nullable
        Checksum checksum;

        @Override
        public Charset getCharset() {
            return charsetName == null ? StandardCharsets.UTF_8 : Charset.forName(charsetName);
        }

        @SuppressWarnings("unchecked")
        @Override
        public SourceFile withCharset(Charset charset) {
            return withCharsetName(charset.name());
        }

        List<Statement> statements;
        Space eof;

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitCompilationUnit(this, p);
        }

        @Override
        public <P> TreeVisitor<?, PrintOutputCapture<P>> printer(Cursor cursor) {
            return new JclPrinter<>();
        }
    }


    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class Assignment implements Jcl, Parameter, Expression {
        @Nullable
        @NonFinal
        transient WeakReference<Padding> padding;

        @With
        @EqualsAndHashCode.Include
        @Getter
        UUID id;

        @With
        @Getter
        Space prefix;

        @With
        @Getter
        Markers markers;

        @With
        @Getter
        Expression variable;

        JclLeftPadded<Expression> assignment;

        public Expression getAssignment() {
            return assignment.getElement();
        }

        public Assignment withAssignment(Expression assignment) {
            return getPadding().withAssignment(this.assignment.withElement(assignment));
        }

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitAssignment(this, p);
        }

        public Padding getPadding() {
            Padding p;
            if (this.padding == null) {
                p = new Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final Assignment t;

            public JclLeftPadded<Expression> getAssignment() {
                return t.assignment;
            }

            public Assignment withAssignment(JclLeftPadded<Expression> assignment) {
                return t.assignment == assignment ? t : new Assignment(t.id, t.prefix, t.markers, t.variable, assignment);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Identifier implements Jcl, Expression, Name {
        @Getter
        @EqualsAndHashCode.Include
        UUID id;

        @Getter
        Space prefix;

        @Getter
        Markers markers;

        String simpleName;

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitIdentifier(this, p);
        }
    }

    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class DataDefinitionStatement implements Jcl {
        @Nullable
        @NonFinal
        transient WeakReference<DataDefinitionStatement.Padding> padding;

        @With
        @EqualsAndHashCode.Include
        @Getter
        UUID id;

        @With
        @Getter
        Space prefix;

        @With
        @Getter
        Markers markers;

        @With
        @Getter
        Name name;

        JclContainer<Parameter> parameters;

        public List<Parameter> getParameters() {
            return parameters.getElements();
        }

        public DataDefinitionStatement withParameters(List<Parameter> parameters) {
            return getPadding().withParameters(requireNonNull(JclContainer.withElementsNullable(this.parameters, parameters)));
        }

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitDataDefinitionStatement(this, p);
        }

        public DataDefinitionStatement.Padding getPadding() {
            DataDefinitionStatement.Padding p;
            if (this.padding == null) {
                p = new DataDefinitionStatement.Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new DataDefinitionStatement.Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final DataDefinitionStatement t;

            public JclContainer<Parameter> getParameters() {
                return t.parameters;
            }

            public DataDefinitionStatement withParameters(JclContainer<Parameter> parameters) {
                return t.parameters == parameters ? t : new DataDefinitionStatement(t.id, t.prefix, t.markers, t.name, parameters);
            }
        }
    }

    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class ExecStatement implements Jcl {
        @Nullable
        @NonFinal
        transient WeakReference<ExecStatement.Padding> padding;

        @With
        @EqualsAndHashCode.Include
        @Getter
        UUID id;

        @With
        @Getter
        Space prefix;

        @With
        @Getter
        Markers markers;

        @With
        @Getter
        Name name;

        JclContainer<Parameter> parameters;

        public List<Parameter> getParameters() {
            return parameters.getElements();
        }

        public ExecStatement withParameters(List<Parameter> parameters) {
            return getPadding().withParameters(requireNonNull(JclContainer.withElementsNullable(this.parameters, parameters)));
        }

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitExecStatement(this, p);
        }

        public ExecStatement.Padding getPadding() {
            ExecStatement.Padding p;
            if (this.padding == null) {
                p = new ExecStatement.Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new ExecStatement.Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final ExecStatement t;

            public JclContainer<Parameter> getParameters() {
                return t.parameters;
            }

            public ExecStatement withParameters(JclContainer<Parameter> parameters) {
                return t.parameters == parameters ? t : new ExecStatement(t.id, t.prefix, t.markers, t.name, parameters);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class JclStatement implements Jcl, Statement {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Name name;
        Jcl statement;

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitJclStatement(this, p);
        }
    }

    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class JobStatement implements Jcl {
        @Nullable
        @NonFinal
        transient WeakReference<JobStatement.Padding> padding;

        @With
        @EqualsAndHashCode.Include
        @Getter
        UUID id;

        @With
        @Getter
        Space prefix;

        @With
        @Getter
        Markers markers;

        @With
        @Getter
        Name name;

        JclContainer<Parameter> parameters;

        public List<Parameter> getParameters() {
            return parameters.getElements();
        }

        public JobStatement withParameters(List<Parameter> parameters) {
            return getPadding().withParameters(requireNonNull(JclContainer.withElementsNullable(this.parameters, parameters)));
        }

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitJobStatement(this, p);
        }

        public JobStatement.Padding getPadding() {
            JobStatement.Padding p;
            if (this.padding == null) {
                p = new JobStatement.Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new JobStatement.Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final JobStatement t;

            public JclContainer<Parameter> getParameters() {
                return t.parameters;
            }

            public JobStatement withParameters(JclContainer<Parameter> parameters) {
                return t.parameters == parameters ? t : new JobStatement(t.id, t.prefix, t.markers, t.name, parameters);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Literal implements Jcl, Parameter, Expression {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        @Nullable
        Object value;

        @Nullable
        String valueSource;

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitLiteral(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class JclName implements Jcl, Expression, Name {
        @Getter
        @EqualsAndHashCode.Include
        UUID id;

        @Getter
        Space prefix;

        @Getter
        Markers markers;

        Jcl.Identifier name;

        @Nullable
        Jcl.Parentheses<JclName> parentheses;

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitJclName(this, p);
        }
    }

    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class OutputStatement implements Jcl, Parameter {
        @Nullable
        @NonFinal
        transient WeakReference<OutputStatement.Padding> padding;

        @With
        @EqualsAndHashCode.Include
        @Getter
        UUID id;

        @With
        @Getter
        Space prefix;

        @With
        @Getter
        Markers markers;

        @With
        @Getter
        Name name;

        JclContainer<Parameter> parameters;

        public List<Parameter> getParameters() {
            return parameters.getElements();
        }

        public OutputStatement withParameters(List<Parameter> parameters) {
            return getPadding().withParameters(requireNonNull(JclContainer.withElementsNullable(this.parameters, parameters)));
        }

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitOutputStatement(this, p);
        }

        public OutputStatement.Padding getPadding() {
            OutputStatement.Padding p;
            if (this.padding == null) {
                p = new OutputStatement.Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new OutputStatement.Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final OutputStatement t;

            public JclContainer<Parameter> getParameters() {
                return t.parameters;
            }

            public OutputStatement withParameters(JclContainer<Parameter> parameters) {
                return t.parameters == parameters ? t : new OutputStatement(t.id, t.prefix, t.markers, t.name, parameters);
            }
        }
    }

    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class Parentheses<J2 extends Jcl> implements Jcl, Expression, Parameter {
        @Nullable
        @NonFinal
        transient WeakReference<Padding<J2>> padding;

        @With
        @EqualsAndHashCode.Include
        @Getter
        UUID id;

        @With
        @Getter
        Space prefix;

        @With
        @Getter
        Markers markers;

        List<JclRightPadded<J2>> trees;

        public List<J2> getTrees() {
            return JclRightPadded.getElements(trees);
        }

        Parentheses<J2> withTrees(List<J2> trees) {
            return getPadding().withTrees(JclRightPadded.withElements(this.trees, trees));
        }

        @Nullable
        @With
        Boolean omitFirstParam;

        public Boolean omitFirstParam() {
            return omitFirstParam != null && omitFirstParam;
        }

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitParentheses(this, p);
        }

        public Padding<J2> getPadding() {
            Padding<J2> p;
            if (this.padding == null) {
                p = new Padding<>(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new Padding<>(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding<J2 extends Jcl> {
            private final Parentheses<J2> t;

            public List<JclRightPadded<J2>> getTrees() {
                return t.trees;
            }

            public Parentheses<J2> withTrees(List<JclRightPadded<J2>> trees) {
                return t.trees == trees ? t : new Parentheses<>(t.id, t.prefix, t.markers, trees, t.omitFirstParam);
            }
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Pend implements Jcl, Parameter {
        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Name name;

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitPend(this, p);
        }
    }

    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class ProcStatement implements Jcl, Parameter {
        @Nullable
        @NonFinal
        transient WeakReference<ProcStatement.Padding> padding;

        @With
        @EqualsAndHashCode.Include
        @Getter
        UUID id;

        @With
        @Getter
        Space prefix;

        @With
        @Getter
        Markers markers;

        @With
        @Getter
        Name name;

        JclContainer<Parameter> parameters;

        public List<Parameter> getParameters() {
            return parameters.getElements();
        }

        public ProcStatement withParameters(List<Parameter> parameters) {
            return getPadding().withParameters(requireNonNull(JclContainer.withElementsNullable(this.parameters, parameters)));
        }

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitProcStatement(this, p);
        }

        public ProcStatement.Padding getPadding() {
            ProcStatement.Padding p;
            if (this.padding == null) {
                p = new ProcStatement.Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new ProcStatement.Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final ProcStatement t;

            public JclContainer<Parameter> getParameters() {
                return t.parameters;
            }

            public ProcStatement withParameters(JclContainer<Parameter> parameters) {
                return t.parameters == parameters ? t : new ProcStatement(t.id, t.prefix, t.markers, t.name, parameters);
            }
        }
    }

    @FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @RequiredArgsConstructor
    @AllArgsConstructor(access = AccessLevel.PRIVATE)
    class SetStatement implements Jcl, Parameter {
        @Nullable
        @NonFinal
        transient WeakReference<SetStatement.Padding> padding;

        @With
        @EqualsAndHashCode.Include
        @Getter
        UUID id;

        @With
        @Getter
        Space prefix;

        @With
        @Getter
        Markers markers;

        @With
        @Getter
        Name name;

        JclContainer<Parameter> parameters;

        public List<Parameter> getParameters() {
            return parameters.getElements();
        }

        public SetStatement withParameters(List<Parameter> parameters) {
            return getPadding().withParameters(requireNonNull(JclContainer.withElementsNullable(this.parameters, parameters)));
        }

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitSetStatement(this, p);
        }

        public SetStatement.Padding getPadding() {
            SetStatement.Padding p;
            if (this.padding == null) {
                p = new SetStatement.Padding(this);
                this.padding = new WeakReference<>(p);
            } else {
                p = this.padding.get();
                if (p == null || p.t != this) {
                    p = new SetStatement.Padding(this);
                    this.padding = new WeakReference<>(p);
                }
            }
            return p;
        }

        @RequiredArgsConstructor
        public static class Padding {
            private final SetStatement t;

            public JclContainer<Parameter> getParameters() {
                return t.parameters;
            }

            public SetStatement withParameters(JclContainer<Parameter> parameters) {
                return t.parameters == parameters ? t : new SetStatement(t.id, t.prefix, t.markers, t.name, parameters);
            }
        }
    }
}
