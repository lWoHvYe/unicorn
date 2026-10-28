-- Idempotency and consistency constraints for concurrent producers.
CREATE UNIQUE INDEX IF NOT EXISTS uk_ai_document_hash_acl_status
    ON ai_document (tenant_id, kb_id, content_hash, status, acl_roles);
CREATE UNIQUE INDEX IF NOT EXISTS uk_ai_version_document_no
    ON ai_document_version (document_id, version_no);
CREATE UNIQUE INDEX IF NOT EXISTS uk_ai_chunk_version_index
    ON ai_chunk (document_id, version_id, chunk_index);
CREATE UNIQUE INDEX IF NOT EXISTS uk_ai_approval_invocation
    ON ai_approval (invocation_id);

CREATE TABLE IF NOT EXISTS ai_chat_request (
    tenant_id VARCHAR(64) NOT NULL,
    session_id VARCHAR(64) NOT NULL,
    request_id VARCHAR(128) NOT NULL,
    message_id VARCHAR(64) NOT NULL,
    status VARCHAR(32) NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (tenant_id, session_id, request_id)
);
