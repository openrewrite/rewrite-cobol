package org.openrewrite.cobol.search;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.With;
import lombok.experimental.FieldDefaults;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Marker;

import java.util.UUID;

@RequiredArgsConstructor
@FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
@Getter
@With
public class SearchResult implements Marker {
    UUID id;

    Type type;

    @Nullable
    String description;

    public enum Type {
        INDICATOR_AREA, COPIED_SOURCE
    }
}
