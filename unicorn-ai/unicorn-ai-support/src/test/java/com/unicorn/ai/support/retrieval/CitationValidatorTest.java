/*
 *    Copyright (c) 2026.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package com.unicorn.ai.support.retrieval;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CitationValidatorTest {

    private final CitationValidator validator = new CitationValidator();
    private final List<Citation> citations = List.of(
            new Citation("d1", "v1", "c1", "a.md", "A", "a", 1.0),
            new Citation("d2", "v2", "c2", "b.md", "B", "b", 0.8));

    @Test
    void acceptsExistingCitationIndexes() {
        assertTrue(validator.valid("按资料 [C1]，另外 [C2] 也相关。", citations));
    }

    @Test
    void rejectsOutOfRangeAndMalformedCitations() {
        assertFalse(validator.valid("只看到了 [C3]。", citations));
        assertEquals(List.of(3), validator.invalidReferences("只看到了 [C3]。", citations));
    }

    @Test
    void acceptsAnswersWithoutCitationTokens() {
        assertTrue(validator.valid("资料不足，无法回答。", citations));
    }
}
