package org.openrewrite.jcl.tree;

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
 * A JCL element that could have trailing space.
 *
 * @param <T>
 */
@FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
@EqualsAndHashCode(callSuper = false, onlyExplicitlyIncluded = true)
@Data
public class JclRightPadded<T> {
    @With
    T element;

    @With
    Space after;

    @With
    Markers markers;

    public JclRightPadded<T> map(UnaryOperator<T> map) {
        return withElement(map.apply(element));
    }

    public enum Location {
        PARENTHESES(Space.Location.PARENTHESES);

        private final Space.Location afterLocation;

        Location(Space.Location afterLocation) {
            this.afterLocation = afterLocation;
        }

        public Space.Location getAfterLocation() {
            return afterLocation;
        }
    }

    public static <T> List<T> getElements(List<JclRightPadded<T>> ls) {
        List<T> list = new ArrayList<>();
        for (JclRightPadded<T> l : ls) {
            T elem = l.getElement();
            list.add(elem);
        }
        return list;
    }

    public static <P extends Jcl> List<JclRightPadded<P>> withElements(List<JclRightPadded<P>> before, List<P> elements) {
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

        List<JclRightPadded<P>> after = new ArrayList<>(elements.size());
        Map<UUID, JclRightPadded<P>> beforeById = before.stream().collect(Collectors
                .toMap(j -> j.getElement().getId(), Function.identity()));

        for (P t : elements) {
            if (beforeById.get(t.getId()) != null) {
                JclRightPadded<P> found = beforeById.get(t.getId());
                after.add(found.withElement(t));
            } else {
                after.add(new JclRightPadded<>(t, Space.EMPTY, Markers.EMPTY));
            }
        }

        return after;
    }

    public static <T> JclRightPadded<T> build(T element) {
        return new JclRightPadded<>(element, Space.EMPTY, Markers.EMPTY);
    }

    @Nullable
    public static <T> JclRightPadded<T> withElement(@Nullable JclRightPadded<T> before, @Nullable T elements) {
        if (before == null) {
            if (elements == null) {
                return null;
            }
            return new JclRightPadded<>(elements, Space.EMPTY, Markers.EMPTY);
        }
        if (elements == null) {
            return null;
        }
        return before.withElement(elements);
    }

    @Override
    public String toString() {
        return "JclRightPadded(element=" + element.getClass().getSimpleName() + ", after=" + after + ')';
    }
}
