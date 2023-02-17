package org.openrewrite.cobol.markers;

import lombok.Value;
import lombok.With;
import org.openrewrite.cobol.tree.Space;
import org.openrewrite.marker.Marker;

import java.util.UUID;

/**
 * COBOL word transformations that increase the length of the current line may cause misalignment in the content area.
 * Whitespace is added in two places to simplify column alignment.
 *  1. The {@link CommentArea} contains whitespace until the end of the line.
 *  2. TemplateWhitespace marker contains whitespace until the next word starts in its current position.
 *
 * I.E. PIC replaced by PICTURE
 * Before:
 *  |000001| | firstWord PIC [some words]         |
 *
 * After:
 *  |000001| | firstWord PICTURE                  |
 *  |      | |               [some words]         |
 *
 * The markers enable the ability to print the original word in place of the transformed word to print either the
 * original word or the transformed word.
 */
@With
@Value
public class ReplaceAdditiveWhitespace implements Marker {
    UUID id;
    Space prefix;
}