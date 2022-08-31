package org.openrewrite.cobol.internal;

import java.util.*;

public class HpTandem implements CobolDialect {
    private final Set<String> separators;
    private final Columns columns;

    public HpTandem() {
        // Currently unknown.
        this.separators = Collections.emptySet();
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
