package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Markers;

import java.util.List;
import java.util.UUID;

@Value
@With
public class Replacement {

    UUID id;
    Markers markers;
    List<OriginalWord> originalWords;
    Type type;

    boolean isCopiedSource;

    @Value
    @With
    public static class OriginalWord {
        Cobol.Word original;
        boolean replacedWithEmpty;
    }

    public enum Type {
        ADDITIVE,
        REDUCTIVE,
        EQUAL
    }
}
