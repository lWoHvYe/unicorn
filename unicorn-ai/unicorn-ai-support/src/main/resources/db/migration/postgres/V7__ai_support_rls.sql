-- PostgreSQL defense-in-depth tenant isolation.
-- The application sets app.tenant_id on each pooled connection before use.
-- ai_api_key is excluded because it is the authentication lookup table;
-- ai_outbox/ai_chat_memory/ai_document_version/ai_eval_case/ai_user_role
-- are system or join tables without a direct tenant_id column.

DO $$
DECLARE
    table_name TEXT;
BEGIN
    FOREACH table_name IN ARRAY ARRAY[
        'ai_api_key_audit', 'ai_knowledge_base', 'ai_document', 'ai_chunk',
        'ai_ingest_job', 'ai_chat_session', 'ai_chat_message', 'ai_chat_request', 'ai_feedback',
        'ai_tool_invocation', 'ai_approval', 'ai_usage_daily', 'ai_order',
        'ai_inventory', 'ai_product', 'ai_tenant_user', 'ai_role', 'ai_quota',
        'ai_audit_event', 'ai_prompt_version', 'ai_model_route', 'ai_eval_dataset',
        'ai_eval_run', 'ai_handoff_ticket', 'ai_knowledge_review'
    ]
    LOOP
        EXECUTE format('ALTER TABLE %I ENABLE ROW LEVEL SECURITY', table_name);
        EXECUTE format('ALTER TABLE %I FORCE ROW LEVEL SECURITY', table_name);
        EXECUTE format('CREATE POLICY tenant_isolation ON %I USING (tenant_id = nullif(current_setting(''app.tenant_id'', true), ''''))', table_name);
    END LOOP;
END $$;
