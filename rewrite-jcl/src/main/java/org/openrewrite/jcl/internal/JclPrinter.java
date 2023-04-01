package org.openrewrite.jcl.internal;

import org.openrewrite.jcl.JclIsoVisitor;
import org.openrewrite.jcl.tree.Jcl;

public class JclPrinter<P> extends JclIsoVisitor<P> {
    @Override
    public Jcl.CompilationUnit visitCompilationUnit(Jcl.CompilationUnit cu, P p) {
        return cu;
    }
}
