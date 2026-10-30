-- Spring AI ChatMemory repository, isolated from the business message transcript.
CREATE TABLE IF NOT EXISTS ai_chat_memory (
    conversation_id VARCHAR(160) NOT NULL,
    message_index INT NOT NULL,
    message_type VARCHAR(32) NOT NULL,
    content TEXT,
    metadata_json TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (conversation_id, message_index)
);
CREATE INDEX IF NOT EXISTS idx_ai_chat_memory_conversation ON ai_chat_memory (conversation_id);
