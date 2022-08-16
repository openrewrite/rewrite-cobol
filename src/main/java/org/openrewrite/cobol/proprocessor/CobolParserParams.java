/*
 * Copyright (C) 2017, Ulrich Wolffgang <ulrich.wolffgang@proleap.io>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 */

package org.openrewrite.cobol.proprocessor;

import lombok.Value;

import java.io.File;
import java.nio.charset.Charset;
import java.util.List;

@Value
public class CobolParserParams {
    Charset charset;
    List<File> copyBookDirectories;
    List<String> copyBookExtensions;
    List<File> copyBookFiles;
    CobolDialect dialect;
    CobolPreprocessor.CobolSourceFormatEnum format;
    boolean ignoreSyntaxErrors;
}
