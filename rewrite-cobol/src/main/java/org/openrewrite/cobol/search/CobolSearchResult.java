package org.openrewrite.cobol.search;

import lombok.*;
import lombok.experimental.FieldDefaults;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.SearchResult;

import java.util.UUID;

@FieldDefaults(makeFinal = true, level = AccessLevel.PRIVATE)
@Getter
public class CobolSearchResult extends SearchResult {

    Type type;

    public CobolSearchResult(UUID id, Type type, @Nullable String description) {
        super(id, description);
        this.type = type;
    }

    public enum Type {
        INDICATOR_AREA, COPIED_SOURCE
    }
}
