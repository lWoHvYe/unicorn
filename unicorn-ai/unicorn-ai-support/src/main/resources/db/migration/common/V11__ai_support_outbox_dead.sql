ALTER TABLE ai_outbox ADD COLUMN IF NOT EXISTS last_error VARCHAR(2048);
ALTER TABLE ai_outbox ADD COLUMN IF NOT EXISTS dead_at TIMESTAMP;
CREATE INDEX IF NOT EXISTS idx_ai_outbox_dead ON ai_outbox (status, dead_at);
