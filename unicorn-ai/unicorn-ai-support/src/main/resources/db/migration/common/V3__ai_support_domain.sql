-- Demo/production support domain tables. Real deployments can point these tables at
-- an external order/inventory service instead; the tool adapters only depend on the
-- repository interfaces in unicorn-ai-tools.
CREATE TABLE IF NOT EXISTS ai_order (
    tenant_id VARCHAR(64) NOT NULL,
    order_id VARCHAR(64) NOT NULL,
    customer VARCHAR(200) NOT NULL,
    status VARCHAR(32) NOT NULL,
    amount NUMERIC(12, 2) NOT NULL,
    created_at VARCHAR(32) NOT NULL,
    days_since_purchase INT NOT NULL DEFAULT 0,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (tenant_id, order_id)
);

CREATE TABLE IF NOT EXISTS ai_inventory (
    tenant_id VARCHAR(64) NOT NULL,
    sku VARCHAR(64) NOT NULL,
    name VARCHAR(200) NOT NULL,
    available INT NOT NULL,
    reserved INT NOT NULL DEFAULT 0,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (tenant_id, sku)
);

CREATE TABLE IF NOT EXISTS ai_product (
    tenant_id VARCHAR(64) NOT NULL,
    sku VARCHAR(64) NOT NULL,
    name VARCHAR(200) NOT NULL,
    price NUMERIC(12, 2) NOT NULL,
    stock INT NOT NULL DEFAULT 0,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (tenant_id, sku)
);

CREATE INDEX IF NOT EXISTS idx_ai_order_tenant ON ai_order (tenant_id, status);
CREATE INDEX IF NOT EXISTS idx_ai_inventory_tenant ON ai_inventory (tenant_id);
CREATE INDEX IF NOT EXISTS idx_ai_product_tenant_name ON ai_product (tenant_id, name);
