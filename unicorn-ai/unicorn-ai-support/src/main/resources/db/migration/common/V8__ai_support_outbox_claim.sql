ALTER TABLE ai_outbox ADD COLUMN IF NOT EXISTS claimed_by VARCHAR(128);
ALTER TABLE ai_outbox ADD COLUMN IF NOT EXISTS claimed_at TIMESTAMP;
CREATE INDEX IF NOT EXISTS idx_ai_outbox_claim ON ai_outbox (status, claimed_by, claimed_at);
