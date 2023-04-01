package org.openrewrite.jcl;

import java.nio.file.Path;

public class JclParsingException extends Exception {
    private final Path sourcePath;

    public JclParsingException(Path sourcePath, String message, Throwable t) {
        super(message, t);
        this.sourcePath = sourcePath;
    }

    public Path getSourcePath() {
        return sourcePath;
    }
}
