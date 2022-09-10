package org.openrewrite.cobol.proleap;

import lombok.Value;

@Value
public class CopyBook {
    String source;
    String processed;
}
