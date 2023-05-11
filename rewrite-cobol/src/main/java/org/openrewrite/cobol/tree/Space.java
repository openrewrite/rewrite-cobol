/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import lombok.EqualsAndHashCode;
import org.openrewrite.internal.lang.Nullable;

import java.util.Map;
import java.util.WeakHashMap;

/**
 * COBOL white space.
 */
@EqualsAndHashCode
@JsonIdentityInfo(generator = ObjectIdGenerators.IntSequenceGenerator.class, property = "@ref")
public class Space {
    public static final Space EMPTY = new Space("");

    @Nullable
    private final String whitespace;

    /*
     * Most occurrences of spaces will have no comments or markers and will be repeated frequently throughout a source file.
     * e.g.: a single space between keywords, or the common indentation of every line in a block.
     * So use flyweights to avoid storing many instances of functionally identical spaces
     */
    private static final Map<String, Space> flyweights = new WeakHashMap<>();

    private Space(@Nullable String whitespace) {
        this.whitespace = whitespace == null || whitespace.isEmpty() ? null : whitespace;
    }

    @JsonCreator
    public static Space build(@Nullable String whitespace) {
        if (whitespace == null || whitespace.isEmpty()) {
            return Space.EMPTY;
        }
        return flyweights.computeIfAbsent(whitespace, k -> new Space(whitespace));
    }

    public String getIndent() {
        return getWhitespaceIndent(whitespace);
    }

    private String getWhitespaceIndent(@Nullable String whitespace) {
        if (whitespace == null) {
            return "";
        }
        int lastNewline = whitespace.lastIndexOf('\n');
        if (lastNewline >= 0) {
            return whitespace.substring(lastNewline + 1);
        } else if (lastNewline == whitespace.length() - 1) {
            return "";
        }
        return whitespace;
    }

    public String getWhitespace() {
        return whitespace == null ? "" : whitespace;
    }


    public Space withWhitespace(String whitespace) {
        if (whitespace.isEmpty()) {
            return Space.EMPTY;
        }

        if (this.whitespace == null || whitespace.equals(this.whitespace)) {
            return this;
        }
        return build(whitespace);
    }

    public boolean isEmpty() {
        return this == EMPTY;
    }

    private static final String[] spaces = {
            "·₁", "·₂", "·₃", "·₄", "·₅", "·₆", "·₇", "·₈", "·₉", "·₊"
    };

    private static final String[] tabs = {
            "-₁", "-₂", "-₃", "-₄", "-₅", "-₆", "-₇", "-₈", "-₉", "-₊"
    };

    @Override
    public String toString() {
        StringBuilder printedWs = new StringBuilder();
        int lastNewline = 0;
        if (whitespace != null) {
            char[] charArray = whitespace.toCharArray();
            for (int i = 0; i < charArray.length; i++) {
                char c = charArray[i];
                if (c == '\n') {
                    printedWs.append("\\n");
                    lastNewline = i + 1;
                } else if (c == '\r') {
                    printedWs.append("\\r");
                    lastNewline = i + 1;
                } else if (c == ' ') {
                    printedWs.append(spaces[(i - lastNewline) % 10]);
                } else if (c == '\t') {
                    printedWs.append(tabs[(i - lastNewline) % 10]);
                }
            }
        }

        return "Space(whitespace='" + printedWs + "')";
    }

    public enum Location {
        // Column areas
        INDICATOR_AREA_PREFIX,
        COMMENT_AREA_PREFIX,
        COMMENT_AREA_EOL,
        // Cobol preprocessor prefixes
        CONTINUATION_PREFIX,
        COMPILER_OPTION_PREFIX,
        COMPILER_OPTIONS_PREFIX,
        COMPILER_XOPTS_PREFIX,
        COPY_BOOK_PREFIX,
        COPY_SOURCE_PREFIX,
        COPY_STATEMENT_PREFIX,
        DIRECTORY_PHRASE_PREFIX,
        EJECT_STATEMENT_PREFIX,
        EXEC_STATEMENT_PREFIX,
        FAMILY_PHRASE_PREFIX,
        PSEUDO_TEXT_PREFIX,
        REPLACE_AREA_PREFIX,
        REPLACE_BY_STATEMENT_PREFIX,
        REPLACE_CLAUSE_PREFIX,
        REPLACE_OFF_STATEMENT_PREFIX,
        REPLACING_PHRASE_PREFIX,
        SKIP_STATEMENT_PREFIX,
        TITLE_STATEMENT_PREFIX,
        PREPROCESSOR_COMPILATION_UNIT_PREFIX,
        PREPROCESSOR_WORD_PREFIX,
        CHAR_DATA_PREFIX,
        CHAR_DATA_LINE_PREFIX,
        CHAR_DATA_SQL_PREFIX,
        // Cobol prefixes
        ABBREVIATION_PREFIX,
        ACCEPT_PREFIX,
        ACCEPT_FROM_DATE_STATEMENT_PREFIX,
        ACCEPT_FROM_ESCAPE_KEY_STATEMENT_PREFIX,
        ACCEPT_FROM_MNEMONIC_STATEMENT_PREFIX,
        ACCEPT_MESSAGE_COUNT_STATEMENT_PREFIX,
        ACCESS_MODE_CLAUSE_PREFIX,
        ADD_PREFIX,
        ADD_CORRESPONDING_PREFIX,
        ADD_TO_PREFIX,
        ADD_TO_GIVING_PREFIX,
        ALPHABET_ALSO_PREFIX,
        ALPHABET_CLAUSE_PREFIX,
        ALPHABET_LITERAL_PREFIX,
        ALPHABET_THROUGH_PREFIX,
        ALTER_PROCEED_TO_PREFIX,
        ALTER_STATEMENT_PREFIX,
        ALTERED_GO_TO_PREFIX,
        ALTERNATE_RECORD_KEY_CLAUSE_PREFIX,
        AND_OR_CONDITION_PREFIX,
        ARGUMENT_PREFIX,
        ARITHMETIC_EXPRESSION_PREFIX,
        ASSIGN_CLAUSE_PREFIX,
        BLOCK_CONTAINS_PREFIX,
        BLOCK_CONTAINS_TO_PREFIX,
        CALL_PREFIX,
        CALL_BY_PREFIX,
        CALL_GIVING_PHRASE_PREFIX,
        CALL_PHRASE_PREFIX,
        CANCEL_PREFIX,
        CANCEL_CALL_PREFIX,
        CHANNEL_CLAUSE_PREFIX,
        CLASS_CLAUSE_PREFIX,
        CLASS_CLAUSE_THROUGH_PREFIX,
        CLASS_CONDITION_PREFIX,
        CLOSE_PREFIX,
        CLOSE_FILE_PREFIX,
        CLOSE_PORT_FILE_IO_STATEMENT_PREFIX,
        CLOSE_PORT_FILE_IO_USING_ASSOCIATED_DATA_PREFIX,
        CLOSE_PORT_FILE_IO_USING_ASSOCIATED_DATA_LENGTH_PREFIX,
        CLOSE_PORT_FILE_IO_USING_CLOSE_DISPOSITION_PREFIX,
        CLOSE_REEL_UNIT_STATEMENT_PREFIX,
        CLOSE_RELATIVE_STATEMENT_PREFIX,
        CLOSE_SET_CLAUSE_PREFIX,
        COLLATING_SEQUENCE_ALPHABET_PREFIX,
        COLLATING_SEQUENCE_CLAUSE_PREFIX,
        COMBINABLE_CONDITION_PREFIX,
        COMMENT_ENTRY_PREFIX,
        COMMITMENT_CONTROL_PREFIX,
        COMMUNICATION_DESCRIPTION_ENTRY_FORMAT_1_PREFIX,
        COMMUNICATION_DESCRIPTION_ENTRY_FORMAT_2_PREFIX,
        COMMUNICATION_DESCRIPTION_ENTRY_FORMAT_3_PREFIX,
        COMMUNICATION_SECTION_PREFIX,
        COMPILATION_UNIT_PREFIX,
        COMPUTE_PREFIX,
        CONDITION_PREFIX,
        CONDITION_NAME_REFERENCE_PREFIX,
        CONDITION_NAME_SUBSCRIPT_REFERENCE_PREFIX,
        CONFIGURATION_SECTION_PREFIX,
        CONTINUE_PREFIX,
        CURRENCY_CLAUSE_PREFIX,
        DATA_ALIGNED_CLAUSE_PREFIX,
        DATA_BASE_SECTION_PREFIX,
        DATA_BASE_SECTION_ENTRY_PREFIX,
        DATA_BLANK_WHEN_ZERO_CLAUSE_PREFIX,
        DATA_COMMON_OWN_LOCAL_CLAUSE_PREFIX,
        DATA_DESCRIPTION_ENTRY_PREFIX,
        DATA_DIVISION_PREFIX,
        DATA_EXTERNAL_CLAUSE_PREFIX,
        DATA_GLOBAL_CLAUSE_PREFIX,
        DATA_INTEGER_STRING_CLAUSE_PREFIX,
        DATA_JUSTIFIED_CLAUSE_PREFIX,
        DATA_OCCURS_CLAUSE_PREFIX,
        DATA_OCCURS_DEPENDING_PREFIX,
        DATA_OCCURS_INDEXED_PREFIX,
        DATA_OCCURS_SORT_PREFIX,
        DATA_OCCURS_TO_PREFIX,
        DATA_PICTURE_CLAUSE_PREFIX,
        DATA_RECEIVED_BY_CLAUSE_PREFIX,
        DATA_RECORD_AREA_CLAUSE_PREFIX,
        DATA_RECORDS_CLAUSE_PREFIX,
        DATA_REDEFINES_CLAUSE_PREFIX,
        DATA_RENAMES_CLAUSE_PREFIX,
        DATA_SIGN_CLAUSE_PREFIX,
        DATA_SYNCHRONIZED_CLAUSE_PREFIX,
        DATA_THREAD_LOCAL_CLAUSE_PREFIX,
        DATA_TYPE_CLAUSE_PREFIX,
        DATA_TYPE_DEF_CLAUSE_PREFIX,
        DATA_USAGE_CLAUSE_PREFIX,
        DATA_USING_CLAUSE_PREFIX,
        DATA_VALUE_CLAUSE_PREFIX,
        DATA_VALUE_INTERVAL_PREFIX,
        DATA_VALUE_INTERVAL_TO_PREFIX,
        DATA_WITH_LOWER_BOUNDS_CLAUSE_PREFIX,
        DECIMAL_POINT_CLAUSE_PREFIX,
        DEFAULT_COMPUTATIONAL_SIGN_CLAUSE_PREFIX,
        DEFAULT_DISPLAY_SIGN_CLAUSE_PREFIX,
        DELETE_PREFIX,
        DESTINATION_COUNT_CLAUSE_PREFIX,
        DESTINATION_TABLE_CLAUSE_PREFIX,
        DISABLE_PREFIX,
        DISPLAY_PREFIX,
        DISPLAY_AT_PREFIX,
        DISPLAY_UPON_PREFIX,
        DIVIDE_PREFIX,
        DIVIDE_GIVING_PREFIX,
        DIVIDE_GIVING_PHRASE_PREFIX,
        DIVIDE_INTO_PREFIX,
        DIVIDE_REMAINDER_PREFIX,
        ENABLE_PREFIX,
        END_KEY_CLAUSE_PREFIX,
        END_PROGRAM_PREFIX,
        ENTRY_PREFIX,
        ENVIRONMENT_DIVISION_PREFIX,
        EVALUATE_PREFIX,
        EVALUATE_ALSO_PREFIX,
        ENVIRONMENT_SWITCH_NAME_CLAUSE_PREFIX,
        ENVIRONMENT_SWITCH_NAME_SPECIAL_NAMES_STATUS_PHRASE_PREFIX,
        ERROR_KEY_CLAUSE_PREFIX,
        EVALUATE_ALSO_CONDITION_PREFIX,
        EVALUATE_CONDITION_PREFIX,
        EVALUATE_THROUGH_PREFIX,
        EVALUATE_VALUE_THROUGH_PREFIX,
        EVALUATE_WHEN_PREFIX,
        EVALUATE_WHEN_PHRASE_PREFIX,
        EXEC_CICS_STATEMENT_PREFIX,
        EXEC_SQL_IMS_STATEMENT_PREFIX,
        EXEC_SQL_STATEMENT_PREFIX,
        EXHIBIT_PREFIX,
        EXIT_PREFIX,
        EXTERNAL_CLAUSE_PREFIX,
        FIGURATIVE_CONSTANT_PREFIX,
        FILE_CONTROL_ENTRY_PREFIX,
        FILE_CONTROL_PARAGRAPH_PREFIX,
        FILE_DESCRIPTION_ENTRY_PREFIX,
        FILE_SECTION_PREFIX,
        FILE_STATUS_CLAUSE_PREFIX,
        FUNCTION_CALL_PREFIX,
        GENERATE_PREFIX,
        GLOBAL_CLAUSE_PREFIX,
        GO_BACK_PREFIX,
        GO_TO_PREFIX,
        GO_TO_DEPENDING_ON_STATEMENT_PREFIX,
        IDENTIFICATION_DIVISION_PREFIX,
        IDENTIFICATION_DIVISION_PARAGRAPH_PREFIX,
        IF_PREFIX,
        IF_ELSE_PREFIX,
        IF_THEN_PREFIX,
        IN_DATA_PREFIX,
        IN_FILE_PREFIX,
        IN_LIBRARY_PREFIX,
        IN_MNEMONIC_PREFIX,
        IN_SECTION_PREFIX,
        IN_TABLE_PREFIX,
        INITIALIZE_PREFIX,
        INITIALIZE_REPLACING_BY_PREFIX,
        INITIALIZE_REPLACING_PHRASE_PREFIX,
        INITIATE_PREFIX,
        INPUT_OUTPUT_SECTION_PREFIX,
        INSPECT_PREFIX,
        INSPECT_ALL_LEADING_PREFIX,
        INSPECT_ALL_LEADINGS_PREFIX,
        INSPECT_BEFORE_AFTER_PREFIX,
        INSPECT_BY_PREFIX,
        INSPECT_CHARACTERS_PREFIX,
        INSPECT_CONVERTING_PHRASE_PREFIX,
        INSPECT_FOR_PREFIX,
        INSPECT_REPLACING_ALL_LEADING_PREFIX,
        INSPECT_REPLACING_ALL_LEADINGS_PREFIX,
        INSPECT_REPLACING_CHARACTERS_PREFIX,
        INSPECT_REPLACING_PHRASE_PREFIX,
        INSPECT_TALLYING_PHRASE_PREFIX,
        INSPECT_TALLYING_REPLACING_PHRASE_PREFIX,
        INSPECT_TO_PREFIX,
        IO_CONTROL_PARAGRAPH_PREFIX,
        LABEL_RECORDS_CLAUSE_PREFIX,
        LIBRARY_ATTRIBUTE_CLAUSE_1_PREFIX,
        LIBRARY_ATTRIBUTE_CLAUSE_2_PREFIX,
        LIBRARY_ATTRIBUTE_FUNCTION_PREFIX,
        LIBRARY_ATTRIBUTE_PARAMETER_PREFIX,
        LIBRARY_ATTRIBUTE_TITLE_PREFIX,
        LIBRARY_DESCRIPTION_ENTRY_FORMAT_1_PREFIX,
        LIBRARY_DESCRIPTION_ENTRY_FORMAT_2_PREFIX,
        LIBRARY_ENTRY_PROCEDURE_CLAUSE_FORMAT_1_PREFIX,
        LIBRARY_ENTRY_PROCEDURE_CLAUSE_FORMAT_2_PREFIX,
        LIBRARY_ENTRY_PROCEDURE_FOR_CLAUSE_PREFIX,
        LIBRARY_ENTRY_PROCEDURE_GIVING_CLAUSE_PREFIX,
        LIBRARY_ENTRY_PROCEDURE_USING_CLAUSE_PREFIX,
        LIBRARY_ENTRY_PROCEDURE_WITH_CLAUSE_PREFIX,
        LIBRARY_IS_COMMON_CLAUSE_PREFIX,
        LIBRARY_IS_GLOBAL_CLAUSE_PREFIX,
        LINAGE_CLAUSE_PREFIX,
        LINAGE_FOOTING_AT_PREFIX,
        LINAGE_LINES_AT_BOTTOM_PREFIX,
        LINAGE_LINES_AT_TOP_PREFIX,
        LINKAGE_SECTION_PREFIX,
        LOCAL_STORAGE_SECTION_PREFIX,
        MERGE_PREFIX,
        MERGE_COLLATING_SEQUENCE_PHRASE_PREFIX,
        MERGE_GIVING_PREFIX,
        MERGE_GIVING_PHRASE_PREFIX,
        MERGE_ON_KEY_CLAUSE_PREFIX,
        MERGE_OUTPUT_PROCEDURE_PHRASE_PREFIX,
        MERGE_OUTPUT_THROUGH_PREFIX,
        MERGE_USING_PREFIX,
        MERGEABLE_PREFIX,
        MESSAGE_COUNT_CLAUSE_PREFIX,
        MESSAGE_DATA_CLAUSE_PREFIX,
        MESSAGE_TIME_CLAUSE_PREFIX,
        MOVE_CORRESPONDING_TO_STATEMENT_PREFIX,
        MOVE_STATEMENT_PREFIX,
        MOVE_TO_STATEMENT_PREFIX,
        MULT_DIV_PREFIX,
        MULT_DIVS_PREFIX,
        MULTIPLE_FILE_CLAUSE_PREFIX,
        MULTIPLE_FILE_POSITION_PREFIX,
        MULTIPLY_PREFIX,
        MULTIPLY_GIVING_PREFIX,
        MULTIPLY_REGULAR_PREFIX,
        NEXT_SENTENCE_PREFIX,
        OBJECT_COMPUTER_PREFIX,
        OBJECT_COMPUTER_DEFINITION_PREFIX,
        ODT_CLAUSE_PREFIX,
        OPEN_PREFIX,
        OPEN_IO_EXTEND_STATEMENT_PREFIX,
        OPEN_INPUT_OUTPUT_STATEMENT_PREFIX,
        OPENABLE_PREFIX,
        ORGANIZATION_CLAUSE_PREFIX,
        PADDING_CHARACTER_CLAUSE_PREFIX,
        PARAGRAPH_PREFIX,
        PARAGRAPHS_PREFIX,
        PARENTHESIZED_PREFIX,
        PASSWORD_CLAUSE_PREFIX,
        PERFORM_PREFIX,
        PERFORM_IN_LINE_STATEMENT_PREFIX,
        PERFORM_PROCEDURE_STATEMENT_PREFIX,
        PERFORM_TEST_CLAUSE_PREFIX,
        PERFORM_TIMES_PREFIX,
        PERFORM_UNTIL_PREFIX,
        PERFORM_VARYING_PREFIX,
        PERFORM_VARYING_CLAUSE_PREFIX,
        PERFORM_VARYING_PHRASE_PREFIX,
        PERFORMABLE_PREFIX,
        PICTURE_PREFIX,
        PICTURE_STRING_PREFIX,
        PLUS_MINUS_PREFIX,
        POWER_PREFIX,
        POWERS_PREFIX,
        PROCEDURE_DECLARATIVE_PREFIX,
        PROCEDURE_DECLARATIVES_PREFIX,
        PROCEDURE_DIVISION_PREFIX,
        PROCEDURE_DIVISION_BODY_PREFIX,
        PROCEDURE_DIVISION_BY_REFERENCE_PREFIX,
        PROCEDURE_DIVISION_BY_REFERENCE_PHRASE_PREFIX,
        PROCEDURE_DIVISION_BY_VALUE_PHRASE_PREFIX,
        PROCEDURE_DIVISION_GIVING_CLAUSE_PREFIX,
        PROCEDURE_DIVISION_USING_CLAUSE_PREFIX,
        PROCEDURE_NAME_PREFIX,
        PROCEDURE_SECTION_PREFIX,
        PROCEDURE_SECTION_HEADER_PREFIX,
        PROGRAM_ID_PARAGRAPH_PREFIX,
        PROGRAM_LIBRARY_SECTION_PREFIX,
        PROGRAM_UNIT_PREFIX,
        PURGE_PREFIX,
        QUALIFIED_DATA_NAME_PREFIX,
        QUALIFIED_DATA_NAME_FORMAT_1_PREFIX,
        QUALIFIED_DATA_NAME_FORMAT_2_PREFIX,
        QUALIFIED_DATA_NAME_FORMAT_3_PREFIX,
        QUALIFIED_DATA_NAME_FORMAT_4_PREFIX,
        QUALIFIED_IN_DATA_PREFIX,
        READ_PREFIX,
        READ_INTO_PREFIX,
        READ_KEY_PREFIX,
        READ_WITH_PREFIX,
        RECEIVABLE_PREFIX,
        RECEIVE_WITH_PREFIX,
        RECEIVE_PREFIX,
        RECEIVE_FROM_PREFIX,
        RECEIVE_FROM_STATEMENT_PREFIX,
        RECEIVE_INTO_STATEMENT_PREFIX,
        RECORD_CONTAINS_CLAUSE_PREFIX,
        RECORD_CONTAINS_CLAUSE_FORMAT_1_PREFIX,
        RECORD_CONTAINS_CLAUSE_FORMAT_2_PREFIX,
        RECORD_CONTAINS_CLAUSE_FORMAT_3_PREFIX,
        RECORD_CONTAINS_TO_PREFIX,
        RECORD_DELIMITER_CLAUSE_PREFIX,
        RECORD_KEY_CLAUSE_PREFIX,
        RECORDING_MODE_CLAUSE_PREFIX,
        REFERENCE_MODIFIER_PREFIX,
        RELATION_ARITHMETIC_COMPARISON_PREFIX,
        RELATION_COMBINED_COMPARISON_PREFIX,
        RELATION_COMBINED_CONDITION_PREFIX,
        RELATION_SIGN_CONDITION_PREFIX,
        RELATIONAL_OPERATOR_PREFIX,
        RELATIVE_KEY_CLAUSE_PREFIX,
        RELEASE_PREFIX,
        REPORT_CLAUSE_PREFIX,
        REPORT_DESCRIPTION_PREFIX,
        REPORT_DESCRIPTION_ENTRY_PREFIX,
        REPORT_DESCRIPTION_FIRST_DETAIL_CLAUSE_PREFIX,
        REPORT_DESCRIPTION_FOOTING_CLAUSE_PREFIX,
        REPORT_DESCRIPTION_GLOBAL_CLAUSE_PREFIX,
        REPORT_DESCRIPTION_HEADING_CLAUSE_PREFIX,
        REPORT_DESCRIPTION_LAST_DETAIL_CLAUSE_PREFIX,
        REPORT_DESCRIPTION_PAGE_LIMIT_CLAUSE_PREFIX,
        REPORT_GROUP_BLANK_WHEN_ZERO_CLAUSE_PREFIX,
        REPORT_GROUP_COLUMN_NUMBER_CLAUSE_PREFIX,
        REPORT_GROUP_DESCRIPTION_ENTRY_FORMAT_1_PREFIX,
        REPORT_GROUP_DESCRIPTION_ENTRY_FORMAT_2_PREFIX,
        REPORT_GROUP_DESCRIPTION_ENTRY_FORMAT_3_PREFIX,
        REPORT_GROUP_INDICATOR_CLAUSE_PREFIX,
        REPORT_GROUP_JUSTIFIED_CLAUSE_PREFIX,
        REPORT_GROUP_LINE_NUMBER_CLAUSE_PREFIX,
        REPORT_GROUP_LINE_NUMBER_NEXT_PAGE_PREFIX,
        REPORT_GROUP_LINE_NUMBER_PLUS_PREFIX,
        REPORT_GROUP_NEXT_GROUP_CLAUSE_PREFIX,
        REPORT_GROUP_NEXT_GROUP_NEXT_PAGE_PREFIX,
        REPORT_GROUP_NEXT_GROUP_PLUS_PREFIX,
        REPORT_GROUP_PICTURE_CLAUSE_PREFIX,
        REPORT_GROUP_RESET_CLAUSE_PREFIX,
        REPORT_GROUP_SIGN_CLAUSE_PREFIX,
        REPORT_GROUP_SOURCE_CLAUSE_PREFIX,
        REPORT_GROUP_SUM_CLAUSE_PREFIX,
        REPORT_GROUP_TYPE_CLAUSE_PREFIX,
        REPORT_GROUP_TYPE_CONTROL_FOOTING_PREFIX,
        REPORT_GROUP_TYPE_CONTROL_HEADING_PREFIX,
        REPORT_GROUP_TYPE_DETAIL_PREFIX,
        REPORT_GROUP_TYPE_PAGE_FOOTING_PREFIX,
        REPORT_GROUP_TYPE_REPORT_FOOTING_PREFIX,
        REPORT_GROUP_TYPE_PAGE_HEADING_PREFIX,
        REPORT_GROUP_TYPE_REPORT_HEADING_PREFIX,
        REPORT_GROUP_USAGE_CLAUSE_PREFIX,
        REPORT_GROUP_VALUE_CLAUSE_PREFIX,
        REPORT_NAME_PREFIX,
        REPORT_SECTION_PREFIX,
        RERUN_CLAUSE_PREFIX,
        RERUN_EVERY_CLOCK_PREFIX,
        RERUN_EVERY_OF_PREFIX,
        RERUN_EVERY_RECORDS_PREFIX,
        RERUN_RESERVE_CLAUSE_PREFIX,
        RESERVE_NETWORK_CLAUSE_PREFIX,
        RETURN_PREFIX,
        RETURN_INTO_PREFIX,
        REWRITE_PREFIX,
        REWRITE_FROM_PREFIX,
        ROUNDABLE_PREFIX,
        SAME_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_AUTO_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_BACKGROUND_COLOR_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_BELL_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_BLANK_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_BLANK_WHEN_ZERO_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_BLINK_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_COLUMN_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_CONTROL_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_ENTRY_PREFIX,
        SCREEN_DESCRIPTION_ERASE_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_FOREGROUND_COLOR_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_FROM_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_FULL_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_GRID_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_JUSTIFIED_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_LIGHT_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_LINE_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_PICTURE_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_PROMPT_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_PROMPT_OCCURS_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_REQUIRED_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_REVERSE_VIDEO_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_SECURE_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_SIGN_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_SIZE_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_TO_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_UNDERLINE_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_USAGE_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_USING_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_VALUE_CLAUSE_PREFIX,
        SCREEN_DESCRIPTION_ZERO_FILL_CLAUSE_PREFIX,
        SCREEN_SECTION_PREFIX,
        SEARCH_PREFIX,
        SEARCH_VARYING_PREFIX,
        SEARCH_WHEN_PREFIX,
        SEARCH_CLAUSE_PREFIX,
        SEND_PREFIX,
        SEND_ADVANCING_LINES_PREFIX,
        SEND_PHRASE_PREFIX,
        SEND_STATEMENT_SYNC_PREFIX,
        SENTENCE_PREFIX,
        SEQUENCE_AREA_PREFIX,
        SET_PREFIX,
        SET_TO_PREFIX,
        SET_UP_DOWN_PREFIX,
        SORT_PREFIX,
        SORT_COLLATING_SEQUENCE_PHRASE_PREFIX,
        SORT_GIVING_PREFIX,
        SORT_PROCEDURE_PHRASE_PREFIX,
        SORTABLE_PREFIX,
        SOURCE_COMPUTER_PREFIX,
        SOURCE_COMPUTER_DEFINITION_PREFIX,
        SPECIAL_NAMES_PREFIX,
        START_PREFIX,
        START_KEY_PREFIX,
        STATEMENT_PHRASE_PREFIX,
        STATUS_KEY_CLAUSE_PREFIX,
        STOP_PREFIX,
        STOP_STATEMENT_GIVING_PREFIX,
        STRING_DELIMITED_BY_PHRASE_PREFIX,
        STRING_FOR_PHRASE_PREFIX,
        STRING_INTO_PHRASE_PREFIX,
        STRING_SENDING_PHRASE_PREFIX,
        STRING_STATEMENT_PREFIX,
        STRING_WITH_POINTER_PHRASE_PREFIX,
        SUBSCRIPT_PREFIX,
        SUBTRACT_PREFIX,
        SUBTRACT_CORRESPONDING_STATEMENT_PREFIX,
        SUBTRACT_FROM_GIVING_STATEMENT_PREFIX,
        SUBTRACT_FROM_STATEMENT_PREFIX,
        SUBTRACT_MINUEND_CORRESPONDING_PREFIX,
        SYMBOLIC_CHARACTER_PREFIX,
        SYMBOLIC_CHARACTERS_CLAUSE_PREFIX,
        SYMBOLIC_DESTINATION_CLAUSE_PREFIX,
        SYMBOLIC_QUEUE_CLAUSE_PREFIX,
        SYMBOLIC_SOURCE_CLAUSE_PREFIX,
        SYMBOLIC_SUB_QUEUE_CLAUSE_PREFIX,
        SYMBOLIC_TERMINAL_CLAUSE_PREFIX,
        TABLE_CLAUSE_PREFIX,
        TERMINATE_PREFIX,
        TEXT_LENGTH_CLAUSE_PREFIX,
        UNSTRING_PREFIX,
        UNSTRING_COUNT_IN_PREFIX,
        UNSTRING_DELIMITED_BY_PHRASE_PREFIX,
        UNSTRING_DELIMITED_IN_PREFIX,
        UNSTRING_INTO_PREFIX,
        UNSTRING_INTO_PHRASE_PREFIX,
        UNSTRING_OR_ALL_PHRASE_PREFIX,
        UNSTRING_SENDING_PHRASE_PREFIX,
        UNSTRING_TALLYING_PHRASE_PREFIX,
        UNSTRING_WITH_POINTER_PHRASE_PREFIX,
        USE_AFTER_CLAUSE_PREFIX,
        USE_AFTER_ON_PREFIX,
        USE_DEBUG_CLAUSE_PREFIX,
        USE_DEBUG_ON_PREFIX,
        USE_STATEMENT_PREFIX,
        VALUE_OF_CLAUSE_PREFIX,
        VALUE_PAIR_PREFIX,
        VALUE_OBJECT_COMPUTER_CLAUSE_PREFIX,
        WORD_PREFIX,
        WORKING_STORAGE_SECTION_PREFIX,
        WRITE_PREFIX,
        WRITE_ADVANCING_LINES_PREFIX,
        WRITE_ADVANCING_MNEMONIC_PREFIX,
        WRITE_ADVANCING_PAGE_PREFIX,
        WRITE_ADVANCING_PHRASE_PREFIX,
        WRITE_FROM_PHRASE_PREFIX,
    }
}
