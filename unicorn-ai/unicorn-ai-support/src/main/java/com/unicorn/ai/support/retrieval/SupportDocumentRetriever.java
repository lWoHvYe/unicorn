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

import org.springframework.ai.document.Document;
import org.springframework.ai.rag.Query;
import org.springframework.ai.rag.retrieval.search.DocumentRetriever;
import com.unicorn.ai.support.observability.SupportMetrics;
import com.unicorn.ai.support.security.AiPrincipal;
import com.unicorn.ai.support.security.TenantContext;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;
import java.util.Set;

@Component
public class SupportDocumentRetriever implements DocumentRetriever {

    private final RetrievalService retrievalService;
    private final SupportMetrics metrics;

    public SupportDocumentRetriever(RetrievalService retrievalService, SupportMetrics metrics) {
        this.retrievalService = retrievalService;
        this.metrics = metrics;
    }

    @SuppressWarnings("unchecked")
    @Override
    public List<Document> retrieve(Query query) {
        var context = query.context();
        var tenantId = Objects.toString(context.get("tenant"), "");
        var kbId = Objects.toString(context.get("kbId"), "");
        if (tenantId.isBlank() || kbId.isBlank()) {
            throw new IllegalStateException("RAG query context requires tenant and kbId");
        }
        var roles = context.get("roles") instanceof Set<?> set
                ? (Set<String>) set : Set.<String>of();
        var capture = context.get("capture");
        if (capture instanceof RetrievalCapture retrievalCapture && retrievalCapture.get() != null) {
            return retrievalCapture.get().documents();
        }
        var started = System.nanoTime();
        var result = TenantContext.callUnchecked(new AiPrincipal(tenantId, "retrieval", roles),
                () -> retrievalService.retrieve(tenantId, kbId, query.text(), null, roles));
        metrics.retrieval(tenantId, result.documents().isEmpty(), System.nanoTime() - started);
        if (capture instanceof RetrievalCapture retrievalCapture) {
            retrievalCapture.set(result);
        }
        return result.documents();
    }
}
