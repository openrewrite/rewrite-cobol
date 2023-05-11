/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree;

import lombok.EqualsAndHashCode;
import lombok.Value;
import lombok.With;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;

import java.util.function.UnaryOperator;

@Value
@EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
@With
public class CobolLeftPadded<T> {
    Space before;
    T element;
    Markers markers;

    public CobolLeftPadded<T> map(UnaryOperator<T> map) {
        return withElement(map.apply(element));
    }

    @Nullable
    public static <T> CobolLeftPadded<T> withElement(@Nullable CobolLeftPadded<T> before, @Nullable T elements) {
        if (before == null) {
            if (elements == null) {
                return null;
            }
            return new CobolLeftPadded<>(Space.EMPTY, elements, Markers.EMPTY);
        }
        if (elements == null) {
            return null;
        }
        return before.withElement(elements);
    }

    @Override
    public String toString() {
        return "ProtoLeftPadded(before=" + before + ", element=" + element.getClass().getSimpleName() + ')';
    }

    public static <T> CobolLeftPadded<T> build(T element) {
        return new CobolLeftPadded<>(Space.EMPTY, element, Markers.EMPTY);
    }
}
