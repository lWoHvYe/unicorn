-- 异步 RAG 入库任务审计表；infra profile 下由 spring.sql.init.mode=always 初始化
CREATE TABLE IF NOT EXISTS ai_rag_ingest_job (
    id VARCHAR(64) PRIMARY KEY,
    source VARCHAR(512) NOT NULL,
    status VARCHAR(32) NOT NULL,
    chunks INT NOT NULL DEFAULT 0,
    error VARCHAR(1024),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);
