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
import com.unicorn.ai.support.retrieval.RetrievalService;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class EvalService {

    private final RetrievalService retrievalService;

    public EvalService(RetrievalService retrievalService) {
        this.retrievalService = retrievalService;
    }

    public EvalReport run(String tenantId, String kbId, List<EvalCase> cases) {
        return run(tenantId, kbId, cases, java.util.Set.of());
    }

    public EvalReport run(String tenantId, String kbId, List<EvalCase> cases, java.util.Set<String> roles) {
        if (cases == null || cases.isEmpty()) {
            throw new IllegalArgumentException("cases 不能为空");
        }
        int hit = 0;
        int reciprocalRankNumerator = 0;
        var details = new java.util.ArrayList<EvalCaseResult>();
        for (var evalCase : cases) {
            var result = retrievalService.retrieve(tenantId, kbId, evalCase.question(), 5, roles);
            var rank = rankOf(result.citations(), evalCase.expectedSource());
            if (rank > 0) {
                hit++;
                reciprocalRankNumerator += 1;
            }
            details.add(new EvalCaseResult(evalCase.question(), evalCase.expectedSource(), rank));
        }
        var recall = (double) hit / cases.size();
        var mrr = (double) reciprocalRankNumerator / cases.size();
        return new EvalReport(cases.size(), hit, recall, mrr, details);
    }

    private int rankOf(List<Citation> citations, String expectedSource) {
        for (int i = 0; i < citations.size(); i++) {
            if (citations.get(i).source() != null && citations.get(i).source().contains(expectedSource)) {
                return i + 1;
            }
        }
        return 0;
    }

    public record EvalCase(String question, String expectedSource) {
    }

    public record EvalCaseResult(String question, String expectedSource, int rank) {
    }

    public record EvalReport(int total, int hits, double recallAt5, double mrr, List<EvalCaseResult> details) {
    }
}
