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

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class InMemoryIngestJobRepositoryTest {

    private final InMemoryIngestJobRepository repository = new InMemoryIngestJobRepository();

    @Test
    void jobLifecycle() {
        repository.createRunning("job-1", "demo/doc");
        var running = repository.find("job-1").orElseThrow();
        assertEquals("RUNNING", running.status());
        assertEquals("demo/doc", running.source());

        repository.complete("job-1", 3);
        var completed = repository.find("job-1").orElseThrow();
        assertEquals("COMPLETED", completed.status());
        assertEquals(3, completed.chunks());
    }

    @Test
    void failedJobKeepsError() {
        repository.createRunning("job-2", "demo/fail");
        repository.fail("job-2", "boom");
        var failed = repository.find("job-2").orElseThrow();
        assertEquals("FAILED", failed.status());
        assertTrue(failed.error().contains("boom"));
    }
}
