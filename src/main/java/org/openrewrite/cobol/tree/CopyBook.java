package org.openrewrite.cobol.tree;

import lombok.Value;
import lombok.With;
import org.openrewrite.Tree;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.nio.file.Path;
import java.util.UUID;

// TODO: Convert to source file.
@Value
@With
public class CopyBook implements CobolPreprocessor {

    UUID id;
    Space prefix;
    Markers markers;

    // ... verbose for quality assurance.
    Path sourcePath;
    String sourceCode;
    CobolPreprocessor ast;
    CobolPreprocessor.Word eof;

    /*
    Notes:
    A CopyBook should be a sourceFile eventually.
    The `CopyStatement` contains a `CopySource` that will be used to associate the statement to the SourceFile.
    Then the sourceFile may be change dn each of the copy statements will be refactored as well.
    Printing the changes as correct COBOL is fairly straight forward, but is not implemented yet.
     */
}
