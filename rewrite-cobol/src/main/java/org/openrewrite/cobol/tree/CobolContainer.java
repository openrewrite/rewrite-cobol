/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree;

import com.fasterxml.jackson.annotation.JsonCreator;
import lombok.RequiredArgsConstructor;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;

import java.util.List;
import java.util.function.UnaryOperator;

import static java.util.Collections.emptyList;

/**
 * AST elements that contain lists of trees with some delimiter like function call arguments.
 *
 * @param <T> The type of the inner list of elements.
 */
public class CobolContainer<T> {
    private transient Padding<T> padding;

    private static final CobolContainer<?> EMPTY = new CobolContainer<>(Space.EMPTY, null, emptyList(), Markers.EMPTY);

    private final Space before;

    @Nullable
    private final CobolLeftPadded<String> preposition;

    private final List<CobolRightPadded<T>> elements;
    private final Markers markers;

    private CobolContainer(Space before, @Nullable CobolLeftPadded<String> preposition,
                           List<CobolRightPadded<T>> elements, Markers markers) {
        this.before = before;
        this.preposition = preposition;
        this.elements = elements;
        this.markers = markers;
    }

    public static <T> CobolContainer<T> build(List<CobolRightPadded<T>> elements) {
        return build(Space.EMPTY, null, elements, Markers.EMPTY);
    }

    @JsonCreator
    public static <T> CobolContainer<T> build(Space before, @Nullable CobolLeftPadded<String> preposition,
                                              List<CobolRightPadded<T>> elements, Markers markers) {
        if (before.isEmpty() && elements.isEmpty()) {
            return empty();
        }
        return new CobolContainer<>(before, preposition, elements, markers);
    }

    @SuppressWarnings("unchecked")
    public static <T> CobolContainer<T> empty() {
        return (CobolContainer<T>) EMPTY;
    }

    public CobolContainer<T> withPreposition(@Nullable CobolLeftPadded<String> preposition) {
        return this.preposition == preposition ? this : build(before, preposition, elements, markers);
    }

    public CobolContainer<T> withBefore(Space before) {
        return this.before == before ? this : build(before, preposition, elements, markers);
    }

    public CobolContainer<T> withElements(List<CobolRightPadded<T>> elements) {
        return this.elements == elements ? this : build(before, preposition, elements, markers);
    }

    public CobolContainer<T> withMarkers(Markers markers) {
        return this.markers == markers ? this : build(before, preposition, elements, markers);
    }

    @Nullable
    public CobolLeftPadded<String> getPreposition() {
        return preposition;
    }

    public Markers getMarkers() {
        return markers;
    }

    public List<T> getElements() {
        return CobolRightPadded.getElements(elements);
    }

    public Space getBefore() {
        return before;
    }

    public CobolContainer<T> map(UnaryOperator<T> map) {
        return getPadding().withElements(ListUtils.map(elements, t -> t.map(map)));
    }

    public Space getLastSpace() {
        return elements.isEmpty() ? Space.EMPTY : elements.get(elements.size() - 1).getAfter();
    }

    public CobolContainer<T> withLastSpace(Space after) {
        return withElements(ListUtils.mapLast(elements, elem -> elem.withAfter(after)));
    }

    public Padding<T> getPadding() {
        if (padding == null) {
            this.padding = new Padding<>(this);
        }
        return padding;
    }

    @RequiredArgsConstructor
    public static class Padding<T> {
        private final CobolContainer<T> c;

        public List<CobolRightPadded<T>> getElements() {
            return c.elements;
        }

        public CobolContainer<T> withElements(List<CobolRightPadded<T>> elements) {
            return c.elements == elements ? c : build(c.before, c.preposition, elements, c.markers);
        }
    }

    @Nullable
    public static <P extends Cobol> CobolContainer<P> withElementsNullable(@Nullable CobolContainer<P> before, @Nullable List<P> elements) {
        if (before == null) {
            if (elements == null || elements.isEmpty()) {
                return null;
            }
            return CobolContainer.build(Space.EMPTY, null, CobolRightPadded.withElements(emptyList(), elements), Markers.EMPTY);
        }
        if (elements == null || elements.isEmpty()) {
            return null;
        }
        return before.getPadding().withElements(CobolRightPadded.withElements(before.elements, elements));
    }

    public static <P extends Cobol> CobolContainer<P> withElements(CobolContainer<P> before, @Nullable List<P> elements) {
        if (elements == null) {
            return before.getPadding().withElements(emptyList());
        }
        return before.getPadding().withElements(CobolRightPadded.withElements(before.elements, elements));
    }

    @Override
    public String toString() {
        return "CobolContainer(before=" + before + ", elementCount=" + elements.size() + ')';
    }
}
