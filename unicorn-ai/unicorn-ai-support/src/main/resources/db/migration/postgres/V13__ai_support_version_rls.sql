ALTER TABLE ai_document_version ENABLE ROW LEVEL SECURITY;
ALTER TABLE ai_document_version FORCE ROW LEVEL SECURITY;
CREATE POLICY tenant_isolation ON ai_document_version
    USING (tenant_id = nullif(current_setting('app.tenant_id', true), ''));
