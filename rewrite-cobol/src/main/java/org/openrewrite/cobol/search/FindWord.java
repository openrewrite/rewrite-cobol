package org.openrewrite.cobol.search;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.openrewrite.ExecutionContext;
import org.openrewrite.Option;
import org.openrewrite.Recipe;
import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.SearchResult;

import java.util.regex.Pattern;

@EqualsAndHashCode(callSuper = true)
@Value
public class FindWord extends Recipe {

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
        return "Find matching words in the source code";
    }

    @Override
    public String getDescription() {
        return "Search for COBOL words based on a search term.";
    }

    @Override
    protected TreeVisitor<?, ExecutionContext> getVisitor() {
        return new SearchForWord(searchTerm, exactMatch);
    }

    private static class SearchForWord extends CobolIsoVisitor<ExecutionContext> {
        private final String searchTerm;

        @Nullable
        private final Pattern pattern;

        public SearchForWord(String searchTerm, @Nullable Boolean exactMatch) {
            this.searchTerm = searchTerm;
            pattern = Boolean.TRUE.equals(exactMatch) ? null : Pattern.compile(searchTerm.toLowerCase());
        }

        @Override
        public Cobol.Word visitWord(Cobol.Word word, ExecutionContext executionContext) {
            if (matches(word.getWord())) {
                return SearchResult.found(word);
            }
            return super.visitWord(word, executionContext);
        }

        private boolean matches(String word) {
            return pattern != null && pattern.matcher(word.toLowerCase()).matches() || pattern == null && word.equals(searchTerm);
        }
    }
}
