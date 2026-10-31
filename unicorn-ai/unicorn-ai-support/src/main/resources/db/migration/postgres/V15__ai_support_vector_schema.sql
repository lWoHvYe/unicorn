-- PgVectorStore qualifies the table but not the vector/uuid types. Keep both
-- extension schemas aligned with Flyway's application schema.
CREATE EXTENSION IF NOT EXISTS vector;
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

DO $$
DECLARE app_schema text := current_schema();
BEGIN
    EXECUTE format('ALTER EXTENSION vector SET SCHEMA %I', app_schema);
    EXECUTE format('ALTER EXTENSION "uuid-ossp" SET SCHEMA %I', app_schema);
END $$;
