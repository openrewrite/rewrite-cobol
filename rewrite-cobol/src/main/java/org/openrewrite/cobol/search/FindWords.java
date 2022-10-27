package org.openrewrite.cobol.search;

import org.openrewrite.cobol.CobolIsoVisitor;
import org.openrewrite.cobol.tree.Cobol;

import java.util.ArrayList;
import java.util.List;

public class FindWords {

    private FindWords() {
    }

    public static List<Cobol.Word> find(Cobol cobol) {
        CobolIsoVisitor<List<Cobol.Word>> visitor = new CobolIsoVisitor<List<Cobol.Word>>() {
            @Override
            public Cobol.Word visitWord(Cobol.Word word, List<Cobol.Word> words) {
                words.add(word);
                return super.visitWord(word, words);
            }
        };
        List<Cobol.Word> results = new ArrayList<>();
        visitor.visit(cobol, results);
        return results;
    }
}
