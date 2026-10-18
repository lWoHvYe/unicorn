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

import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class InMemoryIngestJobRepository implements IngestJobRepository {

    private final ConcurrentMap<String, IngestJob> jobs = new ConcurrentHashMap<>();

    @Override
    public void createRunning(String id, String source) {
        jobs.put(id, IngestJob.running(id, source));
    }

    @Override
    public void complete(String id, int chunks) {
        jobs.computeIfPresent(id, (key, job) -> job.completed(chunks));
    }

    @Override
    public void fail(String id, String error) {
        jobs.computeIfPresent(id, (key, job) -> job.failed(error));
    }

    @Override
    public Optional<IngestJob> find(String id) {
        return Optional.ofNullable(jobs.get(id));
    }
}
