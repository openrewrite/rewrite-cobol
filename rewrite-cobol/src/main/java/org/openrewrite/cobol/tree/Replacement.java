/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
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
