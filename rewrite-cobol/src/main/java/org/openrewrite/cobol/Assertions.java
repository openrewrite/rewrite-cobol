/*
 * Copyright 2022 the original author or authors.
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
package org.openrewrite.cobol;

import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.cobol.internal.IbmAnsi85;
import org.openrewrite.cobol.tree.Cobol;
import org.openrewrite.cobol.tree.CobolPreprocessor;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.test.SourceSpec;
import org.openrewrite.test.SourceSpecs;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Consumer;

import static java.util.Collections.emptyList;

public class Assertions {
    private Assertions() {
    }

    public static SourceSpecs cobol(@Nullable String before) {
        return cobol(before, s -> {
        });
    }

    public static SourceSpecs cobol(@Nullable String before, Consumer<SourceSpec<Cobol.CompilationUnit>> spec) {
        SourceSpec<Cobol.CompilationUnit> cobol =
                new SourceSpec<>(Cobol.CompilationUnit.class,
                        null,
                        CobolParser.builder(),
                        before,
                        null);
        spec.accept(cobol);
        return cobol;
    }

    public static SourceSpecs cobol(@Nullable String before, String after) {
        return cobol(before, after, s -> {
        });
    }

    public static SourceSpecs cobol(@Nullable String before, String after,
                                    Consumer<SourceSpec<Cobol.CompilationUnit>> spec) {
        SourceSpec<Cobol.CompilationUnit> cobol =
                new SourceSpec<>(Cobol.CompilationUnit.class,
                        null,
                        CobolParser.builder(),
                        before,
                        s -> after);
        spec.accept(cobol);
        return cobol;
    }

    public static SourceSpecs cobolCopy(@Nullable String before) {
        return cobolCopy(before, s -> {
        });
    }

    public static SourceSpecs cobolCopy(@Nullable String before, Consumer<SourceSpec<Cobol.CompilationUnit>> spec) {
        List<CobolPreprocessor.CopyBook> copyBooks = getCopyBookSources();

        SourceSpec<Cobol.CompilationUnit> cobol =
                new SourceSpec<>(Cobol.CompilationUnit.class,
                        null,
                        CobolParser.builder().setEnableCopy(true).setEnableReplace(true).setCopyBooks(copyBooks),
                        before,
                        null);
        spec.accept(cobol);
        return cobol;
    }

    public static SourceSpecs cobolCopy(@Nullable String before, String after) {
        return cobolCopy(before, after, s -> {
        });
    }

    public static SourceSpecs cobolCopy(@Nullable String before, String after,
                                    Consumer<SourceSpec<Cobol.CompilationUnit>> spec) {
        List<CobolPreprocessor.CopyBook> copyBooks = getCopyBookSources();

        SourceSpec<Cobol.CompilationUnit> cobol =
                new SourceSpec<>(Cobol.CompilationUnit.class,
                        null,
                        CobolParser.builder().setEnableCopy(true).setEnableReplace(true).setCopyBooks(copyBooks),
                        before,
                        s -> after);
        spec.accept(cobol);
        return cobol;
    }

    public static SourceSpecs cobolPreprocess(@Nullable String before) {
        return cobolPreprocess(before, s -> {
        });
    }

    public static SourceSpecs cobolPreprocess(@Nullable String before, Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec) {
        SourceSpec<CobolPreprocessor.CompilationUnit> cobol =
                new SourceSpec<>(CobolPreprocessor.CompilationUnit.class,
                        null,
                        CobolPreprocessorParser.builder(),
                        before,
                        null);
        spec.accept(cobol);
        return cobol;
    }

    public static SourceSpecs cobolPreprocess(@Nullable String before, String after) {
        return cobolPreprocess(before, after, s -> {
        });
    }

    public static SourceSpecs cobolPreprocess(@Nullable String before, String after,
                                              Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec) {
        SourceSpec<CobolPreprocessor.CompilationUnit> cobol =
                new SourceSpec<>(CobolPreprocessor.CompilationUnit.class,
                        null,
                        CobolPreprocessorParser.builder(),
                        before,
                        s -> after);
        spec.accept(cobol);
        return cobol;
    }

    public static SourceSpecs cobolPreprocessorCopy(@Nullable String before) {
        return cobolPreprocessorCopy(before, s -> {
        });
    }

    public static SourceSpecs cobolPreprocessorCopy(@Nullable String before, Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec) {
        List<CobolPreprocessor.CopyBook> copyBooks = getCopyBookSources();

        SourceSpec<CobolPreprocessor.CompilationUnit> cobol =
                new SourceSpec<>(CobolPreprocessor.CompilationUnit.class,
                        null,
                        CobolPreprocessorParser.builder()
                                .setEnableCopy(true)
                                .setEnableReplace(true)
                                .setCopyBooks(copyBooks),
                        before,
                        null);
        spec.accept(cobol);
        return cobol;
    }

    public static SourceSpecs cobolPreprocessorCopy(@Nullable String before, String after) {
        return cobolPreprocessorCopy(before, after, s -> {
        });
    }

    public static SourceSpecs cobolPreprocessorCopy(@Nullable String before, String after,
                                                    Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> spec) {
        List<CobolPreprocessor.CopyBook> copyBooks = getCopyBookSources();

        SourceSpec<CobolPreprocessor.CompilationUnit> cobol =
                new SourceSpec<>(CobolPreprocessor.CompilationUnit.class,
                        null,
                        CobolPreprocessorParser.builder()
                                .setEnableCopy(true)
                                .setEnableReplace(true)
                                .setCopyBooks(copyBooks),
                        before,
                        s -> after);
        spec.accept(cobol);
        return cobol;
    }

    private static List<CobolPreprocessor.CopyBook> getCopyBookSources() {
        ResourceParser resourceParser = new ResourceParser(Paths.get("").toAbsolutePath(), emptyList(), emptyList());

        try {
            return resourceParser.parseCopyBooks(emptyList(), IbmAnsi85.getInstance(),
                    CobolPreprocessorParser.COPYBOOK_FILE_EXTENSIONS, new InMemoryExecutionContext());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
