/*
 * Copyright 2020 the original author or authors.
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * https://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
