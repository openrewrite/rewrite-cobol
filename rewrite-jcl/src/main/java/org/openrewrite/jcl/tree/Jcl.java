package org.openrewrite.jcl.tree;

import lombok.*;
import org.openrewrite.*;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.JclVisitor;
import org.openrewrite.jcl.internal.JclPrinter;
import org.openrewrite.marker.Markers;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;

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

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class JobStatement implements Jcl {

        @EqualsAndHashCode.Include
        UUID id;

        Space prefix;
        Markers markers;

        Name name;

        // add parameters ...

        @Override
        public <P> Jcl acceptJcl(JclVisitor<P> v, P p) {
            return v.visitJobStatement(this, p);
        }
    }

    @Value
    @EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
    @With
    class Identifier implements Jcl, Name {
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
}
