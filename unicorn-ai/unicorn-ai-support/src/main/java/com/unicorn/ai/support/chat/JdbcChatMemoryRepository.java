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

import org.springframework.ai.chat.memory.ChatMemoryRepository;
import org.springframework.ai.chat.messages.AssistantMessage;
import org.springframework.ai.chat.messages.Message;
import org.springframework.ai.chat.messages.MessageType;
import org.springframework.ai.chat.messages.SystemMessage;
import org.springframework.ai.chat.messages.UserMessage;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class JdbcChatMemoryRepository implements ChatMemoryRepository {

    private final JdbcTemplate jdbc;

    public JdbcChatMemoryRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    @Override
    public List<String> findConversationIds() {
        return jdbc.queryForList("SELECT DISTINCT conversation_id FROM ai_chat_memory LIMIT 1000", String.class);
    }

    @Override
    public List<Message> findByConversationId(String conversationId) {
        var messages = new ArrayList<Message>();
        jdbc.query("""
                        SELECT message_type, content FROM ai_chat_memory
                        WHERE conversation_id = ? ORDER BY message_index
                        """,
                rs -> {
                    var type = rs.getString("message_type");
                    var content = rs.getString("content");
                    if (MessageType.USER.getValue().equals(type)) {
                        messages.add(new UserMessage(content));
                    } else if (MessageType.ASSISTANT.getValue().equals(type)) {
                        messages.add(new AssistantMessage(content));
                    } else if (MessageType.SYSTEM.getValue().equals(type)) {
                        messages.add(new SystemMessage(content));
                    }
                }, conversationId);
        return messages;
    }

    @Override
    public void saveAll(String conversationId, List<Message> messages) {
        jdbc.update("DELETE FROM ai_chat_memory WHERE conversation_id = ?", conversationId);
        var rows = new ArrayList<Object[]>();
        for (int i = 0; i < messages.size(); i++) {
            var message = messages.get(i);
            if (message.getMessageType() == MessageType.TOOL) {
                continue;
            }
            rows.add(new Object[]{conversationId, i, message.getMessageType().getValue(), message.getText(), null});
        }
        jdbc.batchUpdate("""
                INSERT INTO ai_chat_memory (conversation_id, message_index, message_type, content, metadata_json)
                VALUES (?, ?, ?, ?, ?)
                """, rows);
    }

    @Override
    public void deleteByConversationId(String conversationId) {
        jdbc.update("DELETE FROM ai_chat_memory WHERE conversation_id = ?", conversationId);
    }
}
