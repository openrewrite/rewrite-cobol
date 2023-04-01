package org.openrewrite.jcl.internal;

import org.openrewrite.FileAttributes;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.internal.grammar.JCLParser;
import org.openrewrite.jcl.internal.grammar.JCLParserBaseVisitor;
import org.openrewrite.jcl.tree.Jcl;
import org.openrewrite.jcl.tree.Space;
import org.openrewrite.marker.Markers;

import java.nio.charset.Charset;
import java.nio.file.Path;

import static java.util.Collections.emptyList;
import static org.openrewrite.Tree.randomId;
import static org.openrewrite.jcl.tree.Space.EMPTY;

public class JclParserVisitor extends JCLParserBaseVisitor<Jcl> {

    private final Path path;
    private final @Nullable FileAttributes fileAttributes;
    private final String source;
    private final Charset charset;
    private final boolean charsetBomMarked;

    private int cursor = 0;

    public JclParserVisitor(Path path,
                            @Nullable FileAttributes fileAttributes,
                            String source,
                            Charset charset,
                            boolean charsetBomMarked) {
        this.path = path;
        this.fileAttributes = fileAttributes;
        this.source = source;
        this.charset = charset;
        this.charsetBomMarked = charsetBomMarked;
    }


    @Override
    public Jcl.CompilationUnit visitCompilationUnit(JCLParser.CompilationUnitContext ctx) {
        return new Jcl.CompilationUnit(
                randomId(),
                path,
                fileAttributes,
                EMPTY,
                Markers.EMPTY,
                charset.name(),
                charsetBomMarked,
                null,
                emptyList(),
                Space.build(source.substring(cursor))
        );
    }
}
