/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import org.openrewrite.internal.lang.Nullable;

public class FilenameUtils {

    @Nullable
    public static String getBaseName(String filename) {
        return removeExtension(getName(filename));
    }

    @Nullable
    public static String getExtension(@Nullable String fileName) throws IllegalArgumentException {
        if (fileName == null) {
            return null;
        } else {
            int index = indexOfExtension(fileName);
            return index == -1 ? "" : fileName.substring(index + 1);
        }
    }

    @Nullable
    public static String getName(@Nullable String filename) {
        if (filename == null) {
            return null;
        } else {
            int index = indexOfLastSeparator(filename);
            return filename.substring(index + 1);
        }
    }

    @Nullable
    private static int indexOfExtension(@Nullable String filename) {
        if (filename == null) {
            return -1;
        } else {
            int extensionPos = filename.lastIndexOf(46);
            int lastSeparator = indexOfLastSeparator(filename);
            return lastSeparator > extensionPos ? -1 : extensionPos;
        }
    }

    @Nullable
    private static int indexOfLastSeparator(@Nullable String filename) {
        if (filename == null) {
            return -1;
        } else {
            int lastUnixPos = filename.lastIndexOf(47);
            int lastWindowsPos = filename.lastIndexOf(92);
            return Math.max(lastUnixPos, lastWindowsPos);
        }
    }

    @Nullable
    public static String removeExtension(@Nullable String filename) {
        if (filename == null) {
            return null;
        } else {
            int index = indexOfExtension(filename);
            return index == -1 ? filename : filename.substring(0, index);
        }
    }
}
