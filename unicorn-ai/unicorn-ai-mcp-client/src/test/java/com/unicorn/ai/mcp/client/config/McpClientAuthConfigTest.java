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

package com.unicorn.ai.mcp.client.config;

import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.http.HttpRequest;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class McpClientAuthConfigTest {

    @Test
    void addsApiKeyHeader() {
        var customizer = new McpClientAuthConfig().mcpApiKeyCustomizer(
                new McpClientProperties("system", "demo-key", List.of("get_order")));
        var builder = HttpRequest.newBuilder(URI.create("http://localhost:8094/mcp"));
        customizer.customize(builder, "POST", URI.create("http://localhost:8094/mcp"), "",
                io.modelcontextprotocol.common.McpTransportContext.EMPTY);
        assertEquals("demo-key", builder.build().headers().firstValue("X-API-Key").orElse(null));
    }
}
