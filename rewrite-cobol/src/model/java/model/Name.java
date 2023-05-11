/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package model;

/**
 * Either an {@link Identifier} or {@link Literal}.
 */
public interface Name extends Cobol {
    default String getSimpleName() {
        // will be implemented by @Getter in real model
        return null;
    }
}
