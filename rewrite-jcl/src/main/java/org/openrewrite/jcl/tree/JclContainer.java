package org.openrewrite.jcl.tree;

import com.fasterxml.jackson.annotation.JsonCreator;
import lombok.RequiredArgsConstructor;
import org.openrewrite.internal.ListUtils;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Markers;

import java.util.List;
import java.util.function.UnaryOperator;

import static java.util.Collections.emptyList;

/**
 * LST elements that contain lists of trees with some delimiter like function call arguments.
 *
 * @param <T> The type of the inner list of elements.
 */
public class JclContainer<T> {
    private transient Padding<T> padding;

    private static final JclContainer<?> EMPTY = new JclContainer<>(Space.EMPTY, emptyList(), Markers.EMPTY);

    private final Space before;

    private final List<JclRightPadded<T>> elements;
    private final Markers markers;

    private JclContainer(Space before, List<JclRightPadded<T>> elements, Markers markers) {
        this.before = before;
        this.elements = elements;
        this.markers = markers;
    }

    public static <T> JclContainer<T> build(List<JclRightPadded<T>> elements) {
        return build(Space.EMPTY, elements, Markers.EMPTY);
    }

    @JsonCreator
    public static <T> JclContainer<T> build(Space before, List<JclRightPadded<T>> elements, Markers markers) {
        if (before.isEmpty() && elements.isEmpty()) {
            return empty();
        }
        return new JclContainer<>(before, elements, markers);
    }

    @SuppressWarnings("unchecked")
    public static <T> JclContainer<T> empty() {
        return (JclContainer<T>) EMPTY;
    }

    public JclContainer<T> withBefore(Space before) {
        return this.before == before ? this : build(before, elements, markers);
    }

    public JclContainer<T> withElements(List<JclRightPadded<T>> elements) {
        return this.elements == elements ? this : build(before, elements, markers);
    }

    public JclContainer<T> withMarkers(Markers markers) {
        return this.markers == markers ? this : build(before, elements, markers);
    }

    public Markers getMarkers() {
        return markers;
    }

    public List<T> getElements() {
        return JclRightPadded.getElements(elements);
    }

    public Space getBefore() {
        return before;
    }

    public JclContainer<T> map(UnaryOperator<T> map) {
        return getPadding().withElements(ListUtils.map(elements, t -> t.map(map)));
    }

    public Space getLastSpace() {
        return elements.isEmpty() ? Space.EMPTY : elements.get(elements.size() - 1).getAfter();
    }

    public JclContainer<T> withLastSpace(Space after) {
        return withElements(ListUtils.mapLast(elements, elem -> elem.withAfter(after)));
    }


    public enum Location {
        TODO(Space.Location.TODO, JclRightPadded.Location.TODO),
        PARAMETERS(Space.Location.PARAMETERS, JclRightPadded.Location.PARAMETERS);

        private final Space.Location beforeLocation;
        private final JclRightPadded.Location elementLocation;

        Location(Space.Location beforeLocation, JclRightPadded.Location elementLocation) {
            this.beforeLocation = beforeLocation;
            this.elementLocation = elementLocation;
        }

        public Space.Location getBeforeLocation() {
            return beforeLocation;
        }

        public JclRightPadded.Location getElementLocation() {
            return elementLocation;
        }
    }

    public Padding<T> getPadding() {
        if (padding == null) {
            this.padding = new Padding<>(this);
        }
        return padding;
    }

    @RequiredArgsConstructor
    public static class Padding<T> {
        private final JclContainer<T> c;

        public List<JclRightPadded<T>> getElements() {
            return c.elements;
        }

        public JclContainer<T> withElements(List<JclRightPadded<T>> elements) {
            return c.elements == elements ? c : build(c.before, elements, c.markers);
        }
    }

    @Nullable
    public static <P extends Jcl> JclContainer<P> withElementsNullable(@Nullable JclContainer<P> before, @Nullable List<P> elements) {
        if (before == null) {
            if (elements == null || elements.isEmpty()) {
                return null;
            }
            return JclContainer.build(Space.EMPTY, JclRightPadded.withElements(emptyList(), elements), Markers.EMPTY);
        }
        if (elements == null || elements.isEmpty()) {
            return null;
        }
        return before.getPadding().withElements(JclRightPadded.withElements(before.elements, elements));
    }

    public static <P extends Jcl> JclContainer<P> withElements(JclContainer<P> before, @Nullable List<P> elements) {
        if (elements == null) {
            return before.getPadding().withElements(emptyList());
        }
        return before.getPadding().withElements(JclRightPadded.withElements(before.elements, elements));
    }

    @Override
    public String toString() {
        return "JclContainer(before=" + before + ", elementCount=" + elements.size() + ')';
    }
}
