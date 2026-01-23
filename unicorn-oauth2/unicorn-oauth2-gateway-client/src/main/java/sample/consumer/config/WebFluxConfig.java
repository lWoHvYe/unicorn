/*
 *    Copyright (c) 2024-2026.  lWoHvYe(Hongyan Wang)
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

package sample.consumer.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.support.WebClientAdapter;
import org.springframework.web.service.invoker.HttpServiceProxyFactory;
import sample.consumer.client.NumberClient;

@Configuration
public class WebFluxConfig {

    @Bean
    public WebClient.Builder webClientBuilder() {
        return WebClient.builder();
    }

    @Bean
    public NumberClient numberClient(WebClient.Builder builder) {
        var webClient = builder.baseUrl("http://127.0.0.1:8080/res/res-flux").build();
        var adapter = WebClientAdapter.create(webClient);
        var factory = HttpServiceProxyFactory.builderFor(adapter).build();
        return factory.createClient(NumberClient.class);
    }
}
