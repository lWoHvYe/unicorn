-- Multi-tenant AI support platform, portable subset for H2 (local) and PostgreSQL (infra).
CREATE TABLE IF NOT EXISTS ai_tenant (
    id VARCHAR(64) PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    status VARCHAR(32) NOT NULL DEFAULT 'ACTIVE',
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_api_key (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    user_id VARCHAR(64) NOT NULL,
    key_hash VARCHAR(128) NOT NULL UNIQUE,
    roles VARCHAR(512) NOT NULL,
    enabled BOOLEAN NOT NULL DEFAULT TRUE,
    expires_at TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_knowledge_base (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    name VARCHAR(200) NOT NULL,
    description VARCHAR(1024),
    status VARCHAR(32) NOT NULL DEFAULT 'ACTIVE',
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_document (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    kb_id VARCHAR(64) NOT NULL,
    source VARCHAR(512) NOT NULL,
    title VARCHAR(512) NOT NULL,
    doc_type VARCHAR(64) NOT NULL,
    acl_roles VARCHAR(512),
    content_hash VARCHAR(128) NOT NULL,
    status VARCHAR(32) NOT NULL,
    current_version_id VARCHAR(64),
    created_by VARCHAR(64),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_document_version (
    id VARCHAR(64) PRIMARY KEY,
    document_id VARCHAR(64) NOT NULL,
    version_no INT NOT NULL,
    content_hash VARCHAR(128) NOT NULL,
    storage_key VARCHAR(512),
    content_text TEXT,
    status VARCHAR(32) NOT NULL,
    chunk_count INT NOT NULL DEFAULT 0,
    error TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_chunk (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    kb_id VARCHAR(64) NOT NULL,
    document_id VARCHAR(64) NOT NULL,
    version_id VARCHAR(64) NOT NULL,
    chunk_index INT NOT NULL,
    content TEXT NOT NULL,
    token_count INT NOT NULL DEFAULT 0,
    metadata_json TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_ingest_job (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    document_id VARCHAR(64) NOT NULL,
    version_id VARCHAR(64) NOT NULL,
    status VARCHAR(32) NOT NULL,
    attempts INT NOT NULL DEFAULT 0,
    error TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_chat_session (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    user_id VARCHAR(64) NOT NULL,
    channel VARCHAR(64) NOT NULL,
    kb_id VARCHAR(64) NOT NULL DEFAULT 'demo-kb',
    title VARCHAR(512),
    status VARCHAR(32) NOT NULL DEFAULT 'OPEN',
    summary TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_chat_message (
    id VARCHAR(64) PRIMARY KEY,
    session_id VARCHAR(64) NOT NULL,
    tenant_id VARCHAR(64) NOT NULL,
    role VARCHAR(32) NOT NULL,
    content TEXT NOT NULL,
    citations_json TEXT,
    tool_calls_json TEXT,
    usage_json TEXT,
    trace_id VARCHAR(64),
    status VARCHAR(32) NOT NULL DEFAULT 'DONE',
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_feedback (
    id VARCHAR(64) PRIMARY KEY,
    session_id VARCHAR(64) NOT NULL,
    message_id VARCHAR(64),
    tenant_id VARCHAR(64) NOT NULL,
    rating INT NOT NULL,
    comment VARCHAR(1024),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_tool_invocation (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    session_id VARCHAR(64),
    user_id VARCHAR(64) NOT NULL,
    tool_name VARCHAR(128) NOT NULL,
    args_json TEXT,
    result_json TEXT,
    status VARCHAR(32) NOT NULL,
    approval_status VARCHAR(32),
    idempotency_key VARCHAR(256) NOT NULL,
    latency_ms BIGINT,
    trace_id VARCHAR(64),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT uk_ai_tool_idempotency UNIQUE (tenant_id, idempotency_key)
);

CREATE TABLE IF NOT EXISTS ai_approval (
    id VARCHAR(64) PRIMARY KEY,
    invocation_id VARCHAR(64) NOT NULL,
    tenant_id VARCHAR(64) NOT NULL,
    status VARCHAR(32) NOT NULL,
    requested_by VARCHAR(64) NOT NULL,
    decided_by VARCHAR(64),
    reason VARCHAR(1024),
    expires_at TIMESTAMP,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_usage_daily (
    tenant_id VARCHAR(64) NOT NULL,
    usage_date DATE NOT NULL,
    prompt_tokens BIGINT NOT NULL DEFAULT 0,
    completion_tokens BIGINT NOT NULL DEFAULT 0,
    total_tokens BIGINT NOT NULL DEFAULT 0,
    cost DOUBLE PRECISION NOT NULL DEFAULT 0,
    requests BIGINT NOT NULL DEFAULT 0,
    PRIMARY KEY (tenant_id, usage_date)
);

CREATE INDEX IF NOT EXISTS idx_ai_document_tenant_kb ON ai_document (tenant_id, kb_id, status);
CREATE INDEX IF NOT EXISTS idx_ai_version_document ON ai_document_version (document_id, version_no);
CREATE INDEX IF NOT EXISTS idx_ai_chunk_tenant_kb ON ai_chunk (tenant_id, kb_id);
CREATE INDEX IF NOT EXISTS idx_ai_chunk_document ON ai_chunk (document_id, version_id);
CREATE INDEX IF NOT EXISTS idx_ai_job_status ON ai_ingest_job (tenant_id, status, created_at);
CREATE INDEX IF NOT EXISTS idx_ai_session_tenant ON ai_chat_session (tenant_id, updated_at);
CREATE INDEX IF NOT EXISTS idx_ai_message_session ON ai_chat_message (session_id, created_at);
CREATE INDEX IF NOT EXISTS idx_ai_tool_tenant ON ai_tool_invocation (tenant_id, created_at);
CREATE INDEX IF NOT EXISTS idx_ai_approval_status ON ai_approval (tenant_id, status);
