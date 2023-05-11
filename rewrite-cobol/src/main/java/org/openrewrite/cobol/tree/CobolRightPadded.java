/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree;

import lombok.AccessLevel;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.With;
import lombok.experimental.FieldDefaults;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

/**
 * A COBOL element that could have trailing space.
 *
 * @param <T>
 */
@FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
@EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
@Data
public class CobolRightPadded<T> {
    @With
    T element;

    @With
    Space after;

    @With
    Markers markers;

    public CobolRightPadded<T> map(UnaryOperator<T> map) {
        return withElement(map.apply(element));
    }

    public static <T> List<T> getElements(List<CobolRightPadded<T>> ls) {
        List<T> list = new ArrayList<>();
        for (CobolRightPadded<T> l : ls) {
            T elem = l.getElement();
            list.add(elem);
        }
        return list;
    }

    public static <P extends Cobol> List<CobolRightPadded<P>> withElements(List<CobolRightPadded<P>> before, List<P> elements) {
        // a cheaper check for the most common case when there are no changes
        if (elements.size() == before.size()) {
            boolean hasChanges = false;
            for (int i = 0; i < before.size(); i++) {
                if (before.get(i).getElement() != elements.get(i)) {
                    hasChanges = true;
                    break;
                }
            }
            if (!hasChanges) {
                return before;
            }
        }

        List<CobolRightPadded<P>> after = new ArrayList<>(elements.size());
        Map<UUID, CobolRightPadded<P>> beforeById = before.stream().collect(Collectors
                .toMap(j -> j.getElement().getId(), Function.identity()));

        for (P t : elements) {
            if (beforeById.get(t.getId()) != null) {
                CobolRightPadded<P> found = beforeById.get(t.getId());
                after.add(found.withElement(t));
            } else {
                after.add(new CobolRightPadded<>(t, Space.EMPTY, Markers.EMPTY));
            }
        }

        return after;
    }

    public static <T> CobolRightPadded<T> build(T element) {
        return new CobolRightPadded<>(element, Space.EMPTY, Markers.EMPTY);
    }

    @Nullable
    public static <T> CobolRightPadded<T> withElement(@Nullable CobolRightPadded<T> before, @Nullable T elements) {
        if (before == null) {
            if (elements == null) {
                return null;
            }
            return new CobolRightPadded<>(elements, Space.EMPTY, Markers.EMPTY);
        }
        if (elements == null) {
            return null;
        }
        return before.withElement(elements);
    }

    @Override
    public String toString() {
        return "CobolRightPadded(element=" + element.getClass().getSimpleName() + ", after=" + after + ')';
    }
}
