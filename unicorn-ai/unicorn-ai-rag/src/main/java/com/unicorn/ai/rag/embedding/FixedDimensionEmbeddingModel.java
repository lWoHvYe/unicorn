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

package com.unicorn.ai.rag.embedding;

import org.springframework.ai.document.Document;
import org.springframework.ai.embedding.EmbeddingModel;
import org.springframework.ai.embedding.EmbeddingRequest;
import org.springframework.ai.embedding.EmbeddingResponse;

import java.util.List;

/**
 * RedisVectorStore 初始化索引时会调用 embeddingModel.dimensions()；
 * 默认实现会真的发一次 embedding 请求。这里固定返回配置维度，避免启动时依赖 API Key。
 * 实际 embed 调用仍然委托给真实模型。
 */
public class FixedDimensionEmbeddingModel implements EmbeddingModel {

    private final EmbeddingModel delegate;
    private final int dimensions;

    public FixedDimensionEmbeddingModel(EmbeddingModel delegate, int dimensions) {
        this.delegate = delegate;
        this.dimensions = dimensions;
    }

    @Override
    public EmbeddingResponse call(EmbeddingRequest request) {
        return delegate.call(request);
    }

    @Override
    public float[] embed(Document document) {
        return delegate.embed(document);
    }

    @Override
    public float[] embed(String text) {
        return delegate.embed(text);
    }

    @Override
    public List<float[]> embed(List<String> texts) {
        return delegate.embed(texts);
    }

    @Override
    public EmbeddingResponse embedForResponse(List<String> texts) {
        return delegate.embedForResponse(texts);
    }

    @Override
    public int dimensions() {
        return dimensions;
    }
}
