package org.openrewrite.cobol.proprocessor;

import lombok.Value;

@Value
public class CopyBook {
    String source;
    String processed;
}
