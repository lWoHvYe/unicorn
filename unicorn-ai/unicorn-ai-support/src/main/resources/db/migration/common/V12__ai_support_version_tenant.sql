ALTER TABLE ai_document_version ADD COLUMN IF NOT EXISTS tenant_id VARCHAR(64);
UPDATE ai_document_version v
SET tenant_id = d.tenant_id
FROM ai_document d
WHERE v.document_id = d.id AND v.tenant_id IS NULL;
CREATE INDEX IF NOT EXISTS idx_ai_version_tenant ON ai_document_version (tenant_id, document_id);
