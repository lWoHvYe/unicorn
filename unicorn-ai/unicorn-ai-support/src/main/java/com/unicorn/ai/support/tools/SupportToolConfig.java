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

import com.unicorn.ai.tools.domain.InMemoryInventoryRepository;
import com.unicorn.ai.tools.domain.InMemoryOrderRepository;
import com.unicorn.ai.tools.domain.InMemoryProductRepository;
import com.unicorn.ai.tools.domain.SupportToolService;
import com.unicorn.ai.tools.policy.SupportToolCatalog;
import com.unicorn.ai.tools.policy.ToolInvocationService;
import com.unicorn.ai.support.config.SupportProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SupportToolConfig {

    @Bean
    SupportToolService supportToolService() {
        return new SupportToolService(new InMemoryOrderRepository(), new InMemoryInventoryRepository(),
                new InMemoryProductRepository());
    }

    @Bean
    ToolInvocationService toolInvocationService(JdbcToolInvocationRepository repository,
                                                SupportToolService tools,
                                                SupportProperties properties) {
        var service = new ToolInvocationService(repository);
        SupportToolCatalog.register(service, tools, properties.approval().refundThreshold());
        return service;
    }
}
