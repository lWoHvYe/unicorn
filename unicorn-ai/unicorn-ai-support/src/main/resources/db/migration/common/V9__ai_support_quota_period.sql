ALTER TABLE ai_quota ADD COLUMN IF NOT EXISTS period_start DATE;
UPDATE ai_quota SET period_start = CURRENT_DATE WHERE period_start IS NULL;
