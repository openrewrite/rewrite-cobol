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
package org.openrewrite.cobol.tree;

import io.moderne.serialization.TreeSerializer;
import org.openrewrite.InMemoryExecutionContext;
import org.openrewrite.cobol.*;
import org.openrewrite.cobol.internal.IbmAnsi85;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.test.SourceSpec;
import org.openrewrite.test.SourceSpecs;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Consumer;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;

public class ParserAssertions {
    private ParserAssertions() {
    }

    public static SourceSpecs cobol(@Nullable String before) {
        return cobol(before, s -> {
        });
    }

    public static SourceSpecs cobol(@Nullable String before, Consumer<SourceSpec<Cobol.CompilationUnit>> spec) {
        SourceSpec<Cobol.CompilationUnit> cobol =
                new SourceSpec<>(Cobol.CompilationUnit.class,
                        null,
                        CobolParser.builder()
                                .setEnableCopy(false)
                                .setEnableReplace(false),
                        before,
                        null);
        spec.andThen(isFullyParsed()).andThen(isSerializable()).accept(cobol);
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
                        CobolParser.builder()
                                .setEnableCopy(false)
                                .setEnableReplace(false),
                        before,
                        s -> after);
        spec.andThen(isFullyParsed()).andThen(isSerializable()).accept(cobol);
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
                        CobolParser.builder()
                                .setCopyBooks(copyBooks),
                        before,
                        null);
        spec.andThen(isFullyParsed()).andThen(isSerializable()).accept(cobol);
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
                        CobolParser.builder()
                                .setCopyBooks(copyBooks),
                        before,
                        s -> after);
        spec.andThen(isFullyParsed()).andThen(isSerializable()).accept(cobol);
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
                        CobolPreprocessorParser.builder()
                                .setEnableCopy(false)
                                .setEnableReplace(false),
                        before,
                        null);
        spec.andThen(isPreprocessorFullyParsed()).accept(cobol);
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
                        CobolPreprocessorParser.builder()
                                .setEnableCopy(false)
                                .setEnableReplace(false),
                        before,
                        s -> after);
        spec.andThen(isPreprocessorFullyParsed()).accept(cobol);
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
                                .setCopyBooks(copyBooks),
                        before,
                        null);
        spec.andThen(isPreprocessorFullyParsed()).accept(cobol);
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
                                .setCopyBooks(copyBooks),
                        before,
                        s -> after);
        spec.andThen(isPreprocessorFullyParsed()).accept(cobol);
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

    public static Consumer<SourceSpec<CobolPreprocessor.CompilationUnit>> isPreprocessorFullyParsed() {
        return spec -> spec.afterRecipe(cu -> new CobolPreprocessorIsoVisitor<Integer>() {
            @Override
            public Space visitSpace(Space space, Space.Location loc, Integer integer) {
                String whitespace = space.getWhitespace().trim();
                if (!(IbmAnsi85.getInstance().getSeparators().contains(whitespace + " ") || whitespace.isEmpty())) {
                    return space.withWhitespace("(~~>" + whitespace + "<~~)");
                }
                return super.visitSpace(space, loc, integer);
            }
        }.visit(cu, 0));
    }

    public static Consumer<SourceSpec<Cobol.CompilationUnit>> isFullyParsed() {
        return spec -> spec.afterRecipe(cu -> new CobolIsoVisitor<Integer>() {
            @Override
            public Space visitSpace(Space space, Space.Location loc, Integer integer) {
                String whitespace = space.getWhitespace().trim();
                if (!(IbmAnsi85.getInstance().getSeparators().contains(whitespace + " ") || whitespace.isEmpty())) {
                    return space.withWhitespace("(~~>" + whitespace + "<~~)");
                }
                return super.visitSpace(space, loc, integer);
            }
        }.visit(cu, 0));
    }

    public static Consumer<SourceSpec<Cobol.CompilationUnit>> isSerializable() {
        return spec -> spec.afterRecipe(cu -> {
            // TODO: fix ... serialization issue due to dependencies.
//            try {
//                ByteArrayOutputStream bos = new ByteArrayOutputStream();
//                TreeSerializer serializer = new TreeSerializer();
//                serializer.write(singletonList(cu), bos);
//                InputStream is = new ByteArrayInputStream(bos.toByteArray());
//                serializer.read(is);
//            } catch (IOException e) {
//                throw new RuntimeException(e);
//            }
        });
    }
}
