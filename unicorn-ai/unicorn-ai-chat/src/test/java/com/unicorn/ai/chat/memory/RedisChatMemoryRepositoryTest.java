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

package com.unicorn.ai.chat.memory;

import org.junit.jupiter.api.Test;
import org.springframework.ai.chat.messages.AssistantMessage;
import org.springframework.ai.chat.messages.SystemMessage;
import org.springframework.ai.chat.messages.UserMessage;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class RedisChatMemoryRepositoryTest {

    @Test
    void encodeDecodeRoundTrip() {
        var messages = List.of(
                new UserMessage("你好\n带换行和 | 竖线"),
                new AssistantMessage("收到"),
                new SystemMessage("system prompt"));

        for (var message : messages) {
            var decoded = RedisChatMemoryRepository.decode(RedisChatMemoryRepository.encode(message));
            assertEquals(message.getMessageType(), decoded.getMessageType());
            assertEquals(message.getText(), decoded.getText());
        }
    }

    @Test
    void unknownPayloadFallsBackToUserMessage() {
        var decoded = RedisChatMemoryRepository.decode("not-a-valid-payload");
        assertEquals("not-a-valid-payload", decoded.getText());
    }
}
