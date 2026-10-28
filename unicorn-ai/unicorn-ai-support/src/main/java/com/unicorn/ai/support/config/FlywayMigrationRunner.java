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

package com.unicorn.ai.support.config;

import org.flywaydb.core.Flyway;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import javax.sql.DataSource;
import java.util.Arrays;

/**
 * Boot 4 no longer auto-configures Flyway through the JDBC starter; run migrations explicitly
 * before demo seeding and before the app is considered ready.
 */
@Order(0)
@Component
public class FlywayMigrationRunner implements ApplicationRunner {

    private final DataSource dataSource;
    private final String locations;
    private final String schemas;
    private final String defaultSchema;

    public FlywayMigrationRunner(DataSource dataSource,
                                 @Value("${spring.flyway.locations:classpath:db/migration/common}") String locations,
                                 @Value("${spring.flyway.schemas:}") String schemas,
                                 @Value("${spring.flyway.default-schema:}") String defaultSchema) {
        this.dataSource = dataSource;
        this.locations = locations;
        this.schemas = schemas;
        this.defaultSchema = defaultSchema;
    }

    @Override
    public void run(ApplicationArguments args) {
        var configuration = Flyway.configure()
                .dataSource(dataSource)
                .locations(Arrays.stream(locations.split(",")).map(String::trim).toArray(String[]::new))
                .baselineOnMigrate(true)
                .cleanDisabled(true);
        if (!schemas.isBlank()) {
            configuration.schemas(schemas.split(","));
            configuration.createSchemas(true);
        }
        if (!defaultSchema.isBlank()) {
            configuration.defaultSchema(defaultSchema);
        }
        configuration.load().migrate();
    }
}
