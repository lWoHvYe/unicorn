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

package com.unicorn.ai.rag.job;

import java.time.Instant;

public record IngestJob(String id, String source, String status, int chunks,
                        String error, Instant createdAt, Instant updatedAt) {

    public static IngestJob running(String id, String source) {
        var now = Instant.now();
        return new IngestJob(id, source, "RUNNING", 0, null, now, now);
    }

    public IngestJob completed(int chunks) {
        return new IngestJob(id, source, "COMPLETED", chunks, null, createdAt, Instant.now());
    }

    public IngestJob failed(String error) {
        return new IngestJob(id, source, "FAILED", chunks, error, createdAt, Instant.now());
    }
}
