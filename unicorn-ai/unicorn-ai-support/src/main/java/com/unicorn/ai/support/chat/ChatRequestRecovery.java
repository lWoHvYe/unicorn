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

package com.unicorn.ai.support.chat;

import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class ChatRequestRecovery {

    private final ChatRepository repository;

    public ChatRequestRecovery(ChatRepository repository) {
        this.repository = repository;
    }

    @Scheduled(fixedDelayString = "${ai.support.chat-request.recovery-ms:30000}")
    public void recover() {
        var recovered = repository.failExpiredRequests();
        if (recovered > 0) {
            log.warn("Marked {} expired chat requests as FAILED", recovered);
        }
    }
}
