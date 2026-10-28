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

package com.unicorn.ai.support.tools;

import com.unicorn.ai.support.chat.ChatDtos;

import java.util.function.Consumer;

/**
 * Per-stream bridge from synchronous tool execution to the SSE event sink.
 */
public final class ToolEventContext {

    private static final ThreadLocal<Consumer<ChatDtos.StreamEvent>> SINK = new ThreadLocal<>();

    private ToolEventContext() {
    }

    public static void set(Consumer<ChatDtos.StreamEvent> sink) {
        SINK.set(sink);
    }

    public static void clear() {
        SINK.remove();
    }

    public static void emit(String event, Object data) {
        var sink = SINK.get();
        if (sink != null) {
            sink.accept(new ChatDtos.StreamEvent(event, data));
        }
    }
}
