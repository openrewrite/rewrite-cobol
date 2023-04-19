package org.openrewrite.analysis.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.*;
import org.openrewrite.cobol.CobolPreprocessorVisitor;
import org.openrewrite.cobol.NameVisitor;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.jcl.JclIsoVisitor;
import org.openrewrite.jcl.tree.Jcl;
import org.openrewrite.marker.SearchResult;

import java.util.regex.Pattern;

@EqualsAndHashCode(callSuper = true)
@Value
public class FindReference extends Recipe {

    @Option(displayName = "Term to search for",
            description = "A word or regex pattern to find. By default the search term is case insensitive.",
            example = "CM102M or cm1.*")
    String searchTerm;

    @Nullable
    @Option(displayName = "Only match exact word",
            description = "Search for a word based on an exact match of the search term.",
            example = "true")
    Boolean exactMatch;

    @Override
    public String getDisplayName() {
        return "Find matching identifiers in COBOL, CopyBooks, and JCL";
    }

    @Override
    public String getDescription() {
        return "Finds an identifier by an exact match or regex pattern in COBOL, CopyBooks, and/or JCL.";
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new TreeVisitor<Tree, ExecutionContext>() {
            private final CobolReference cobolReference = new CobolReference();
            private final CopyBookReference copyBookReference = new CopyBookReference();
            private final JclReference jclReference = new JclReference();

            @Nullable
            private final Pattern pattern = Boolean.TRUE.equals(exactMatch) ? null : Pattern.compile(searchTerm.toLowerCase());

            @Override
            public boolean isAcceptable(SourceFile sourceFile, ExecutionContext executionContext) {
                return cobolReference.isAcceptable(sourceFile, executionContext) ||
                        copyBookReference.isAcceptable(sourceFile, executionContext) ||
                        jclReference.isAcceptable(sourceFile, executionContext);
            }

            @Override
            public @Nullable Tree visit(@Nullable Tree tree, ExecutionContext executionContext) {
                if (tree instanceof Cobol) {
                    return cobolReference.visit(tree, executionContext);
                } else if (tree instanceof CobolPreprocessor.CopyBook) {
                    return copyBookReference.visit(tree, executionContext);
                } else if (tree instanceof Jcl.CompilationUnit) {
                    return jclReference.visit(tree, executionContext);
                }
                return super.visit(tree, executionContext);
            }

            class CobolReference extends NameVisitor<ExecutionContext> {
                @Override
                public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
                    if (matches(word.getWord())) {
                        return SearchResult.found(word);
                    }
                    return super.visitWord(word, executionContext);
                }
            }

            class CopyBookReference extends CobolPreprocessorVisitor<ExecutionContext> {
                @Override
                public CobolPreprocessor visitWord(CobolPreprocessor.Word word, ExecutionContext executionContext) {
                    if (matches(word.getCobolWord().getWord())) {
                        return SearchResult.found(word);
                    }
                    return super.visitWord(word, executionContext);
                }
            }

            class JclReference extends JclIsoVisitor<ExecutionContext> {

                @Override
                public Jcl.JclName visitJclName(Jcl.JclName jclName, ExecutionContext executionContext) {
                    if (matches(jclName.getName().getSimpleName())) {
                        return SearchResult.found(jclName);
                    }
                    return super.visitJclName(jclName, executionContext);
                }

                @Override
                public Jcl.Identifier visitIdentifier(Jcl.Identifier identifier, ExecutionContext executionContext) {
                    if (matches(identifier.getSimpleName())) {
                        return SearchResult.found(identifier);
                    }
                    return super.visitIdentifier(identifier, executionContext);
                }
            }

            private boolean matches(String word) {
                return pattern != null && pattern.matcher(word.toLowerCase()).matches() || pattern == null && word.equals(searchTerm);
            }
        };
    }
}
