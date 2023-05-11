/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
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
