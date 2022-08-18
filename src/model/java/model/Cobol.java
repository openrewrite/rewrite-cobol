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
package model;

import generate.Skip;

public interface Cobol {

    @Skip
    class ArithmeticExpression {}
    @Skip
    class Subscript {}
    @Skip
    class CobolWord {}
    @Skip
    class QualifiedDataName {}
    @Skip
    class StatementPhrase {}
    @Skip
    class Parenthesized {}
    @Skip
    class Condition {}
    @Skip
    class ProcedureName {}
    @Skip
    class PictureString {}

}
