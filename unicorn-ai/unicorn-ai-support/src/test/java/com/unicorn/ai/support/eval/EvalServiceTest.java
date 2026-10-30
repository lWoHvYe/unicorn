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

package com.unicorn.ai.support.eval;

import com.unicorn.ai.support.retrieval.Citation;
import com.unicorn.ai.support.retrieval.RetrievalResult;
import com.unicorn.ai.support.retrieval.RetrievalService;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class EvalServiceTest {

    @Test
    void passesWhenGoldenRetrievalMeetsThresholds() {
        var retrieval = mock(RetrievalService.class);
        when(retrieval.retrieve(eq("demo"), eq("kb"), eq("q1"), anyInt(), any()))
                .thenReturn(result("faq.md"));
        var report = new EvalService(retrieval, 0.8, 0.6)
                .run("demo", "kb", List.of(new EvalService.EvalCase("q1", "faq.md")), Set.of());
        assertTrue(report.passed());
    }

    @Test
    void failsWhenGoldenSourceIsMissing() {
        var retrieval = mock(RetrievalService.class);
        when(retrieval.retrieve(eq("demo"), eq("kb"), eq("q1"), anyInt(), any()))
                .thenReturn(result("other.md"));
        var report = new EvalService(retrieval, 0.8, 0.6)
                .run("demo", "kb", List.of(new EvalService.EvalCase("q1", "faq.md")), Set.of());
        assertFalse(report.passed());
    }

    private RetrievalResult result(String source) {
        var citation = new Citation("d", "v", "c", source, "T", "s", 1.0);
        return new RetrievalResult(List.of(), List.of(citation), true);
    }
}
