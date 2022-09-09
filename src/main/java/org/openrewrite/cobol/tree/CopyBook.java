package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;

import java.nio.file.Path;
import java.util.UUID;

@Value
@With
public class CopyBook implements Marker {
    UUID id;
    Path sourcePath;
    String name;
}
