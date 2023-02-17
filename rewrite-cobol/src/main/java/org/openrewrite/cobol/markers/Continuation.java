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
package org.openrewrite.cobol.markers;

import lombok.Value;
import lombok.With;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.Markers;

import java.util.Map;
import java.util.UUID;

/**
 * Continuations contain the Column area markers that need to be printed at specific indexes in a continued String
 * literal, COBOL keyword or COBOL words.
 * <p>
 * I.E. String literal:
 *   |000000| | Some COBOL "string literal ...             |
 *   |000001|-|    "is continued with prefixed whitespace."|
 * <p>
 * I.E. COBOL keyword:
 *   |000000| | C                                          |
 *   |000001|-|  O                                         |
 *   |000002|-|   P                                        |
 *   |000003|-|    Y                                       |
 */
@With
@Value
public class Continuation implements Marker {
    UUID id;
    Map<Integer, Markers> continuations;
}
