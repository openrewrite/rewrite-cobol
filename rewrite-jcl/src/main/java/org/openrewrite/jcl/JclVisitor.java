package org.openrewrite.jcl;

import org.openrewrite.TreeVisitor;
import org.openrewrite.jcl.tree.Jcl;
import org.openrewrite.jcl.tree.Space;

public class JclVisitor<P> extends TreeVisitor<Jcl, P> {

    public Jcl visitCompilationUnit(Jcl.CompilationUnit compilationUnit, P p) {
        Jcl.CompilationUnit c = compilationUnit;
        c = c.withPrefix(visitSpace(c.getPrefix(), Space.Location.COMPILATION_UNIT_PREFIX, p));
        c = c.withMarkers(visitMarkers(c.getMarkers(), p));
        return c;
    }

    public Space visitSpace(Space space, Space.Location location, P p) {
        return space;
    }
}
