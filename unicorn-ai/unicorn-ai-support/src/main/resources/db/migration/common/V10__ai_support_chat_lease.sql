ALTER TABLE ai_chat_request ADD COLUMN IF NOT EXISTS expires_at TIMESTAMP;
ALTER TABLE ai_chat_request ADD COLUMN IF NOT EXISTS lease_owner VARCHAR(64);
CREATE INDEX IF NOT EXISTS idx_ai_chat_request_expiry ON ai_chat_request (status, expires_at);
