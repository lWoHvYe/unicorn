-- PRD-level IAM, governance, audit, eval and outbox tables.
CREATE TABLE IF NOT EXISTS ai_tenant_user (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    external_subject VARCHAR(256) NOT NULL,
    display_name VARCHAR(200),
    email VARCHAR(320),
    status VARCHAR(32) NOT NULL DEFAULT 'ACTIVE',
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT uk_ai_tenant_user_subject UNIQUE (tenant_id, external_subject)
);

CREATE TABLE IF NOT EXISTS ai_role (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    name VARCHAR(64) NOT NULL,
    scopes VARCHAR(1024) NOT NULL DEFAULT '',
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT uk_ai_role_name UNIQUE (tenant_id, name)
);

CREATE TABLE IF NOT EXISTS ai_user_role (
    user_id VARCHAR(64) NOT NULL,
    role_id VARCHAR(64) NOT NULL,
    PRIMARY KEY (user_id, role_id)
);

CREATE TABLE IF NOT EXISTS ai_api_key_audit (
    id VARCHAR(64) PRIMARY KEY,
    api_key_id VARCHAR(64) NOT NULL,
    tenant_id VARCHAR(64) NOT NULL,
    action VARCHAR(32) NOT NULL,
    actor VARCHAR(64),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_quota (
    tenant_id VARCHAR(64) NOT NULL,
    metric VARCHAR(64) NOT NULL,
    period VARCHAR(32) NOT NULL,
    quota_limit BIGINT NOT NULL,
    used BIGINT NOT NULL DEFAULT 0,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (tenant_id, metric, period)
);

CREATE TABLE IF NOT EXISTS ai_outbox (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    aggregate_type VARCHAR(64) NOT NULL,
    aggregate_id VARCHAR(64) NOT NULL,
    event_type VARCHAR(128) NOT NULL,
    payload_json TEXT NOT NULL,
    status VARCHAR(32) NOT NULL DEFAULT 'PENDING',
    attempts INT NOT NULL DEFAULT 0,
    next_attempt_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_audit_event (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    actor_id VARCHAR(64),
    action VARCHAR(128) NOT NULL,
    resource_type VARCHAR(64) NOT NULL,
    resource_id VARCHAR(128),
    trace_id VARCHAR(64),
    detail_json TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_prompt_version (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    scene VARCHAR(64) NOT NULL,
    version INT NOT NULL,
    template TEXT NOT NULL,
    status VARCHAR(32) NOT NULL DEFAULT 'DRAFT',
    created_by VARCHAR(64),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT uk_ai_prompt_scene_version UNIQUE (tenant_id, scene, version)
);

CREATE TABLE IF NOT EXISTS ai_model_route (
    tenant_id VARCHAR(64) NOT NULL,
    scene VARCHAR(64) NOT NULL,
    primary_model VARCHAR(128) NOT NULL,
    fallback_model VARCHAR(128),
    timeout_ms INT NOT NULL DEFAULT 30000,
    max_tokens INT NOT NULL DEFAULT 4096,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (tenant_id, scene)
);

CREATE TABLE IF NOT EXISTS ai_eval_dataset (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    name VARCHAR(200) NOT NULL,
    description VARCHAR(1024),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_eval_case (
    id VARCHAR(64) PRIMARY KEY,
    dataset_id VARCHAR(64) NOT NULL,
    question TEXT NOT NULL,
    expected_source VARCHAR(512),
    expected_answer TEXT,
    forbidden_content TEXT,
    acl_roles VARCHAR(512),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_eval_run (
    id VARCHAR(64) PRIMARY KEY,
    dataset_id VARCHAR(64) NOT NULL,
    tenant_id VARCHAR(64) NOT NULL,
    status VARCHAR(32) NOT NULL,
    metrics_json TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    completed_at TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_handoff_ticket (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    session_id VARCHAR(64) NOT NULL,
    requester_id VARCHAR(64),
    reason VARCHAR(1024) NOT NULL,
    status VARCHAR(32) NOT NULL DEFAULT 'OPEN',
    assignee_id VARCHAR(64),
    priority VARCHAR(16) NOT NULL DEFAULT 'NORMAL',
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ai_knowledge_review (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(64) NOT NULL,
    document_id VARCHAR(64) NOT NULL,
    version_id VARCHAR(64),
    status VARCHAR(32) NOT NULL DEFAULT 'PENDING',
    reviewer_id VARCHAR(64),
    comment VARCHAR(1024),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT uk_ai_review_document_version UNIQUE (document_id, version_id)
);

CREATE INDEX IF NOT EXISTS idx_ai_outbox_status ON ai_outbox (status, next_attempt_at);
CREATE INDEX IF NOT EXISTS idx_ai_audit_tenant_time ON ai_audit_event (tenant_id, created_at);
CREATE INDEX IF NOT EXISTS idx_ai_ticket_tenant_status ON ai_handoff_ticket (tenant_id, status, updated_at);
CREATE INDEX IF NOT EXISTS idx_ai_eval_case_dataset ON ai_eval_case (dataset_id);
CREATE INDEX IF NOT EXISTS idx_ai_review_tenant_status ON ai_knowledge_review (tenant_id, status);
