package org.openrewrite.cobol.internal;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public class IbmAnsi85 implements CobolDialect {
    private final Set<String> separators;
    private final Columns columns;

    public IbmAnsi85() {
        this.separators = new HashSet<>(Arrays.asList(", ", "; "));
        this.columns = Columns.IBM_ANSI_85;
    }

    @Override
    public Collection<String> getSeparators() {
        return separators;
    }

    @Override
    public Columns getColumns() {
        return columns;
    }
}
