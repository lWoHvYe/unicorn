-- PostgreSQL-only extensions and indexes. Applied only by the infra profile.
CREATE EXTENSION IF NOT EXISTS vector;
CREATE EXTENSION IF NOT EXISTS pg_trgm;

CREATE INDEX IF NOT EXISTS idx_ai_chunk_content_trgm
    ON ai_chunk USING GIN (content gin_trgm_ops);
