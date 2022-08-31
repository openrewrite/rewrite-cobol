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
package org.openrewrite.cobol.internal;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.openrewrite.FileAttributes;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorBaseVisitor;
import org.openrewrite.cobol.internal.grammar.CobolPreprocessorParser;
import org.openrewrite.cobol.tree.*;
import org.openrewrite.internal.lang.Nullable;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.openrewrite.Tree.randomId;
import static org.openrewrite.cobol.tree.Space.format;

public class CobolPreprocessorParserVisitor extends CobolPreprocessorBaseVisitor<Object> {

    private static final String COMMENT_ENTRY_TAG = "*>CE ";

    private final Path path;

    @Nullable
    private final FileAttributes fileAttributes;

    private final String source;
    private final Charset charset;
    private final boolean charsetBomMarked;
    private final CobolDialect cobolDialect;

    // TODO: Areas may be a Set of Integer to reduce memory, each method to create the marker would generate the string.
    private final Map<Integer, String> sequenceAreas = new HashMap<>();
    private final Map<Integer, String> indicatorAreas = new HashMap<>();
    private final Map<Integer, String> commentAreas = new HashMap<>();
    private final Set<String> separators = new HashSet<>();
    private int cursor = 0;

    public CobolPreprocessorParserVisitor(Path path, @Nullable FileAttributes fileAttributes,
                                          String source, Charset charset, boolean charsetBomMarked, CobolDialect cobolDialect) {
        this.path = path;
        this.fileAttributes = fileAttributes;
        this.source = source;
        this.charset = charset;
        this.charsetBomMarked = charsetBomMarked;
        this.cobolDialect = cobolDialect;
    }

    public <T> T visit(@Nullable ParseTree... trees) {
        for (ParseTree tree : trees) {
            if (tree != null) {
                //noinspection unchecked
                return (T) visit(tree);
            }
        }
        throw new IllegalStateException("Expected one of the supplied trees to be non-null");
    }

    public <T> T visitNullable(@Nullable ParseTree tree) {
        if (tree == null) {
            //noinspection ConstantConditions
            return null;
        }
        //noinspection unchecked
        return (T) super.visit(tree);
    }

    @Override
    public CobolPreprocessor.CompilationUnit visitStartRule(CobolPreprocessorParser.StartRuleContext ctx) {
        return visitCompilationUnit(ctx.compilationUnit());
    }

    private void init() {
        String[] parts = source.split("\n");

        if (cobolDialect.getColumns() == CobolDialect.Columns.IBM_ANSI_85) {
            CobolDialect.Columns columns = cobolDialect.getColumns();
            int pos = 0;
            for (String part : parts) {
                boolean isCRLF = part.endsWith("\r");
                String cleanedPart = isCRLF ? part.substring(0, part.length() - 1) : part;

                String sequenceArea = cleanedPart.substring(columns.getSequenceArea(), columns.getIndicatorArea());
                sequenceAreas.put(pos, sequenceArea);
                pos += sequenceArea.length();

                String indicatorArea = cleanedPart.substring(columns.getIndicatorArea(), columns.getContentArea());
                indicatorAreas.put(pos, indicatorArea);
                pos += indicatorArea.length();

                String contentArea = cleanedPart.substring(columns.getContentArea(), columns.getOtherArea());
                pos += contentArea.length();

                String otherArea = cleanedPart.length() > columns.getContentArea() ? cleanedPart.substring(columns.getOtherArea()) : "";
                if (!otherArea.isEmpty()) {
                    commentAreas.put(pos, otherArea);
                    pos += otherArea.length();
                }
                pos += isCRLF ? 2 : 1; // Increment for new line delimiter.
            }

            // TODO: separators should likely be modeled.
            separators.addAll(Arrays.asList(", ", "; "));
        } else if (cobolDialect.getColumns() == CobolDialect.Columns.HP_TANDEM) {
            throw new UnsupportedOperationException("Implement me.");
        } else {
            throw new UnsupportedOperationException("CobolDialect is not supported: " + cobolDialect.getColumns().name());
        }
    }

    @Override
    public Object visitCharData(CobolPreprocessorParser.CharDataContext ctx) {
        return super.visitCharData(ctx);
    }

    @Override
    public Object visitCharDataKeyword(CobolPreprocessorParser.CharDataKeywordContext ctx) {
        // Pass through.
        return super.visitCharDataKeyword(ctx);
    }

    @Override
    public Object visitCharDataLine(CobolPreprocessorParser.CharDataLineContext ctx) {
        return super.visitCharDataLine(ctx);
    }

    @Override
    public Object visitCharDataSql(CobolPreprocessorParser.CharDataSqlContext ctx) {
        return super.visitCharDataSql(ctx);
    }

    @Override
    public Object visitCobolWord(CobolPreprocessorParser.CobolWordContext ctx) {
        // Pass through.
        return super.visitCobolWord(ctx);
    }

    @Override
    public Object visitCompilerOption(CobolPreprocessorParser.CompilerOptionContext ctx) {
        return super.visitCompilerOption(ctx);
    }

    @Override
    public Object visitCompilerOptions(CobolPreprocessorParser.CompilerOptionsContext ctx) {
        return super.visitCompilerOptions(ctx);
    }

    @Override
    public Object visitCompilerXOpts(CobolPreprocessorParser.CompilerXOptsContext ctx) {
        return super.visitCompilerXOpts(ctx);
    }

    @Override
    public CobolPreprocessor.CompilationUnit visitCompilationUnit(CobolPreprocessorParser.CompilationUnitContext ctx) {
        init();

        return new CobolPreprocessor.CompilationUnit(
                randomId(),
                path,
                fileAttributes,
                Space.EMPTY,
                Markers.EMPTY,
                charset.name(),
                charsetBomMarked,
                null,
                convertAll(ctx.charDataLine(),
                        ctx.compilerOptions(),
                        ctx.copyStatement(),
                        ctx.execCicsStatement(),
                        ctx.execSqlStatement(),
                        ctx.execSqlImsStatement(),
                        ctx.ejectStatement(),
                        ctx.replaceArea(),
                        ctx.replaceOffStatement(),
                        ctx.skipStatement(),
                        ctx.titleStatement())
        );
    }

    @Override
    public Object visitCopyLibrary(CobolPreprocessorParser.CopyLibraryContext ctx) {
        // Pass through.
        return super.visitCopyLibrary(ctx);
    }

    @Override
    public Object visitCopySource(CobolPreprocessorParser.CopySourceContext ctx) {
        return super.visitCopySource(ctx);
    }

    @Override
    public Object visitCopyStatement(CobolPreprocessorParser.CopyStatementContext ctx) {
        return super.visitCopyStatement(ctx);
    }

    @Override
    public Object visitDirectoryPhrase(CobolPreprocessorParser.DirectoryPhraseContext ctx) {
        return super.visitDirectoryPhrase(ctx);
    }

    @Override
    public Object visitEjectStatement(CobolPreprocessorParser.EjectStatementContext ctx) {
        return super.visitEjectStatement(ctx);
    }

    @Override
    public Object visitExecCicsStatement(CobolPreprocessorParser.ExecCicsStatementContext ctx) {
        return super.visitExecCicsStatement(ctx);
    }

    @Override
    public Object visitExecSqlStatement(CobolPreprocessorParser.ExecSqlStatementContext ctx) {
        return super.visitExecSqlStatement(ctx);
    }

    @Override
    public Object visitExecSqlImsStatement(CobolPreprocessorParser.ExecSqlImsStatementContext ctx) {
        return super.visitExecSqlImsStatement(ctx);
    }

    @Override
    public Object visitFamilyPhrase(CobolPreprocessorParser.FamilyPhraseContext ctx) {
        return super.visitFamilyPhrase(ctx);
    }

    @Override
    public Object visitFilename(CobolPreprocessorParser.FilenameContext ctx) {
        // Pass through.
        return super.visitFilename(ctx);
    }

    @Override
    public Object visitLiteral(CobolPreprocessorParser.LiteralContext ctx) {
        // Pass through.
        return super.visitLiteral(ctx);
    }

    @Override
    public Object visitPseudoText(CobolPreprocessorParser.PseudoTextContext ctx) {
        return super.visitPseudoText(ctx);
    }

    @Override
    public Object visitReplaceable(CobolPreprocessorParser.ReplaceableContext ctx) {
        // Pass through.
        return super.visitReplaceable(ctx);
    }

    @Override
    public Object visitReplaceArea(CobolPreprocessorParser.ReplaceAreaContext ctx) {
        return super.visitReplaceArea(ctx);
    }

    @Override
    public Object visitReplaceByStatement(CobolPreprocessorParser.ReplaceByStatementContext ctx) {
        return super.visitReplaceByStatement(ctx);
    }

    @Override
    public Object visitReplaceClause(CobolPreprocessorParser.ReplaceClauseContext ctx) {
        return super.visitReplaceClause(ctx);
    }

    @Override
    public Object visitReplacement(CobolPreprocessorParser.ReplacementContext ctx) {
        // Pass through.
        return super.visitReplacement(ctx);
    }

    @Override
    public Object visitReplaceOffStatement(CobolPreprocessorParser.ReplaceOffStatementContext ctx) {
        return super.visitReplaceOffStatement(ctx);
    }

    @Override
    public Object visitReplacingPhrase(CobolPreprocessorParser.ReplacingPhraseContext ctx) {
        return super.visitReplacingPhrase(ctx);
    }

    @Override
    public Object visitSkipStatement(CobolPreprocessorParser.SkipStatementContext ctx) {
        return super.visitSkipStatement(ctx);
    }

    @Override
    public Object visitTerminal(TerminalNode node) {
        return super.visitTerminal(node);
    }

    @Override
    public Object visitTitleStatement(CobolPreprocessorParser.TitleStatementContext ctx) {
        return super.visitTitleStatement(ctx);
    }

    private Space whitespace() {
        String prefix = source.substring(cursor, indexOfNextNonWhitespace(cursor, source));
        cursor += prefix.length();
        return format(prefix);
    }

    private int indexOfNextNonWhitespace(int cursor, String source) {
        int delimIndex = cursor;
        for (; delimIndex < source.length(); delimIndex++) {
            if (source.length() > delimIndex + 1) {
                // TODO: explain.
                if (separators.contains(source.substring(delimIndex, delimIndex + 2))) {
                    continue;
                }
            }

            // TODO: explain.
            if (!Character.isWhitespace(source.substring(delimIndex, delimIndex + 1).charAt(0)) ||
                    sequenceAreas.containsKey(delimIndex) || indicatorAreas.containsKey(delimIndex) || commentAreas.containsKey(delimIndex)) {
                break; // found it!
            }
        }
        return delimIndex;
    }

    private @Nullable List<Cobol.Word> wordsList(TerminalNode... wordNodes) {
        List<Cobol.Word> words = new ArrayList<>(wordNodes.length);
        for (TerminalNode wordNode : wordNodes) {
            if (wordNode != null) {
                Cobol.Word cw = (Cobol.Word) visit(wordNode);
                words.add(cw);
            }
        }

        if (words.isEmpty()) {
            return null;
        }

        return words;
    }

    private <C, T extends ParseTree> List<C> convertAll(List<T> trees, Function<T, C> convert) {
        List<C> converted = new ArrayList<>(trees.size());
        for (T tree : trees) {
            converted.add(convert.apply(tree));
        }
        return converted;
    }

    @SafeVarargs
    private final <C extends Cobol> List<C> convertAll(List<? extends ParserRuleContext>... trees) {
        return convertAll(Arrays.stream(trees)
                .filter(Objects::nonNull)
                .flatMap(Collection::stream)
                .sorted(Comparator.comparingInt(it -> it.start.getStartIndex()))
                .collect(Collectors.toList()));
    }

    private <C extends Cobol, T extends ParseTree> List<C> convertAll(List<T> trees) {
        //noinspection unchecked
        return convertAll(trees, t -> (C) visit(t));
    }

    @SafeVarargs
    private final List<Cobol> convertAllList(List<? extends ParseTree>... trees) {
        return Arrays.stream(trees)
                .flatMap(Collection::stream)
                .filter(Objects::nonNull)
                .sorted(Comparator.comparingInt(it -> it instanceof TerminalNode ? ((TerminalNode) it).getSymbol().getStartIndex() :
                        ((ParserRuleContext) it).getStart().getStartIndex()))
                .map(it -> (Cobol) visit(it))
                .collect(Collectors.toList());
    }

    /**
     * Return the prefix of the TerminalNode AND collect applicable markers.
     * Markers consist of COBOL areas that are removed during preprocessing.
     */
    private Space processTokenText(String text, List<Marker> markers) {

        Character delimiter = null;
        if (text.startsWith("'") || text.startsWith("\"")) {
            delimiter = text.charAt(0);
        }

        String current = delimiter == null ? null : source.substring(source.indexOf(delimiter));
        // Detect a literal continued on a new line.
        if (current != null &&
                current.length() > text.length() &&
                !current.substring(0, current.substring(1).indexOf(delimiter) + 2).equals(text)) {
            return processLiteral(text, markers, delimiter);
        }

        return processText(text, markers);
    }

    /**
     * TODO: explain
     */
    private Space processLiteral(String text, List<Marker> markers, Character delimiter) {
        Map<Integer, Markers> continuations = new HashMap<>();
        List<Marker> continuation = new ArrayList<>(2);

        SequenceArea sequenceArea = sequenceArea();
        IndicatorArea indicatorArea = indicatorArea(null);

        if (indicatorArea != null) {
            String contentArea = source.substring(cursor, cursor - cobolDialect.getColumns().getIndicatorArea() - 1 + cobolDialect.getColumns().getOtherArea());
            if (contentArea.trim().isEmpty() || "*".equals(indicatorArea.getIndicator())) {
                cursor += contentArea.length();
                List<Lines.Line> lines = new ArrayList<>();
                Lines.Line line = new Lines.Line(randomId(), sequenceArea, indicatorArea, contentArea, commentArea());
                lines.add(line);

                int iterations = 0;
                while (iterations < 200) {
                    sequenceArea = sequenceArea();
                    indicatorArea = indicatorArea(null);
                    contentArea = source.substring(cursor, cursor - cobolDialect.getColumns().getIndicatorArea() - 1 + cobolDialect.getColumns().getOtherArea());
                    if (contentArea.trim().isEmpty() || indicatorArea != null && "*".equals(indicatorArea.getIndicator())) {
                        cursor += contentArea.length();
                        line = new Lines.Line(randomId(), sequenceArea, indicatorArea, contentArea, commentArea());
                        lines.add(line);

                        sequenceArea = null;
                        indicatorArea = null;
                    } else {
                        break;
                    }
                    iterations++;
                }

                markers.add(new Lines(randomId(), lines));
            }
        }

        if (sequenceArea != null) {
            continuation.add(sequenceArea);
        }

        if (indicatorArea != null) {
            continuation.add(indicatorArea);
        }

        Space prefix = whitespace();
        if (!continuation.isEmpty()) {
            continuations.put(0, Markers.build(continuation));
        }

        int matchedCount = 0;
        int iterations = 0;
        while (matchedCount < text.length() && iterations < 200) {
            continuation = new ArrayList<>(3);

            String current = source.substring(cursor);
            char[] charArray = text.substring(matchedCount).toCharArray();
            char[] sourceArray = current.toCharArray();

            int end = 0;
            for (; end < charArray.length; end++) {
                if (charArray[end] != sourceArray[end] || commentAreas.containsKey(cursor)) {
                    break;
                }
                cursor++;
            }

            String matchedText = current.substring(0, end);
            matchedCount += matchedText.length();

            CommentArea commentArea = commentArea();
            if (commentArea != null) {
                continuation.add(commentArea);
            }

            int saveCursor = cursor;
            sequenceArea = sequenceArea();
            indicatorArea = indicatorArea(delimiter);

            // Note: "-" might not be safe for all COBOL dialects.
            if (indicatorArea != null && !indicatorArea.getIndicator().startsWith("-")) {
                if (!continuation.isEmpty()) {
                    continuations.put(text.length() + 1, Markers.build(continuation));
                }
                cursor = saveCursor;
                break;
            } else {
                if (sequenceArea != null) {
                    continuation.add(sequenceArea);
                }
                if (indicatorArea != null) {
                    continuation.add(indicatorArea);
                }
            }

            if (!continuation.isEmpty()) {
                continuations.put(matchedCount, Markers.build(continuation));
            }

            iterations++;
        }

        markers.add(new Continuation(randomId(), continuations));
        return prefix;
    }

    /**
     * TODO: explain
     */
    private Space processText(String text, List<Marker> markers) {
        SequenceArea sequenceArea = sequenceArea();
        IndicatorArea indicatorArea = indicatorArea(null);

        boolean isCommentEntry = text.startsWith(COMMENT_ENTRY_TAG);
        if (isCommentEntry) {
            text = text.substring(COMMENT_ENTRY_TAG.length());
        }

        if (!isCommentEntry && indicatorArea != null) {
            String contentArea = source.substring(cursor, cursor - cobolDialect.getColumns().getIndicatorArea() - 1 + cobolDialect.getColumns().getOtherArea());
            if (contentArea.trim().isEmpty() || "*".equals(indicatorArea.getIndicator())) {
                cursor += contentArea.length();
                List<Lines.Line> lines = new ArrayList<>();
                Lines.Line line = new Lines.Line(randomId(), sequenceArea, indicatorArea, contentArea, commentArea());
                lines.add(line);

                int iterations = 0;
                while (iterations < 200) {
                    sequenceArea = sequenceArea();
                    indicatorArea = indicatorArea(null);
                    contentArea = source.substring(cursor, cursor - cobolDialect.getColumns().getIndicatorArea() - 1 + cobolDialect.getColumns().getOtherArea());
                    if (contentArea.trim().isEmpty() || indicatorArea != null && "*".equals(indicatorArea.getIndicator())) {
                        cursor += contentArea.length();
                        line = new Lines.Line(randomId(), sequenceArea, indicatorArea, contentArea, commentArea());
                        lines.add(line);

                        sequenceArea = null;
                        indicatorArea = null;
                    } else {
                        break;
                    }
                    iterations++;
                }

                markers.add(new Lines(randomId(), lines));
            }
        }

        if (sequenceArea != null) {
            markers.add(sequenceArea);
        }

        if (indicatorArea != null) {
            markers.add(indicatorArea);
        }

        // An inline comment entry will have a null sequence area.
        Space prefix = isCommentEntry ? Space.EMPTY : whitespace();
        cursor += text.length();

        CommentArea commentArea = commentArea();
        if (commentArea != null) {
            markers.add(commentArea);
        }
        return prefix;
    }

    /**
     * TODO: explain
     */
    @Nullable
    private SequenceArea sequenceArea() {
        if (sequenceAreas.containsKey(cursor)) {
            String sequence = sequenceAreas.get(cursor);
            cursor += sequence.length();

            return new SequenceArea(randomId(), sequence);
        }
        return null;
    }

    /**
     * TODO: explain
     */
    @Nullable
    private IndicatorArea indicatorArea(@Nullable Character delimiter) {
        if (indicatorAreas.containsKey(cursor)) {
            String indicatorArea = indicatorAreas.get(cursor);
            if (delimiter != null) {
                // Increment passed the start of the literal.
                String current = source.substring(cursor + 1);
                int pos = current.indexOf(delimiter);
                if (pos != -1) {
                    indicatorArea = indicatorArea + current.substring(0, current.indexOf(delimiter) + 1);
                }
            }
            cursor += indicatorArea.length();

            return new IndicatorArea(randomId(), indicatorArea);
        }
        return null;
    }

    /**
     * TODO: explain
     */
    @Nullable
    private CommentArea commentArea() {
        int saveCursor = cursor;

        Space before = whitespace();
        String comment = null;
        Space endLine = Space.EMPTY;

        if (commentAreas.containsKey(cursor)) {
            comment = commentAreas.get(cursor);

            cursor += comment.length();
            endLine = whitespace();
        }

        // Ensure the last whitespace is added to the ASt.
        if (before.getWhitespace().endsWith("\n") || comment != null) {
            return new CommentArea(randomId(), before, comment == null ? "" : comment, endLine);
        }

        cursor = saveCursor;
        return null;
    }
}
