/*
 *    Copyright (c) 2026.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package com.unicorn.ai.support.tools;

import com.unicorn.ai.tools.domain.ProductRepository;
import com.unicorn.ai.tools.model.Product;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class JdbcProductRepository implements ProductRepository {

    private final JdbcTemplate jdbc;

    public JdbcProductRepository(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    @Override
    public List<Product> search(String tenantId, String keyword) {
        var pattern = "%" + (keyword == null ? "" : keyword.toLowerCase()) + "%";
        return jdbc.query("""
                        SELECT tenant_id, sku, name, price, stock
                        FROM ai_product
                        WHERE tenant_id = ? AND (LOWER(name) LIKE ? OR LOWER(sku) LIKE ?)
                        ORDER BY sku
                        LIMIT 20
                        """,
                (rs, rowNum) -> new Product(rs.getString("tenant_id"), rs.getString("sku"),
                        rs.getString("name"), rs.getBigDecimal("price"), rs.getInt("stock")),
                tenantId, pattern, pattern);
    }
}
