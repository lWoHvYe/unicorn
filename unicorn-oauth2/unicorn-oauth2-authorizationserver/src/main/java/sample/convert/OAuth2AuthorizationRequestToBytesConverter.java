/*
 * Copyright 2020-2024 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package sample.convert;

import org.springframework.core.convert.converter.Converter;
import org.springframework.data.convert.WritingConverter;
import org.springframework.data.redis.serializer.JacksonJsonRedisSerializer;
import org.springframework.security.jackson.SecurityJacksonModules;
import org.springframework.security.oauth2.core.endpoint.OAuth2AuthorizationRequest;
import org.springframework.security.oauth2.server.authorization.jackson.OAuth2AuthorizationServerJacksonModule;
import tools.jackson.databind.json.JsonMapper;

@WritingConverter
public class OAuth2AuthorizationRequestToBytesConverter implements Converter<OAuth2AuthorizationRequest, byte[]> {

    private final JacksonJsonRedisSerializer<OAuth2AuthorizationRequest> serializer;

    public OAuth2AuthorizationRequestToBytesConverter() {
        var objectMapper = JsonMapper.builder()
                .addModules(SecurityJacksonModules.getModules(OAuth2AuthorizationRequestToBytesConverter.class.getClassLoader()))
                .addModules(new OAuth2AuthorizationServerJacksonModule())
                .build();
        this.serializer = new JacksonJsonRedisSerializer<>(objectMapper, OAuth2AuthorizationRequest.class);
    }

    @Override
    public byte[] convert(OAuth2AuthorizationRequest value) {
        return this.serializer.serialize(value);
    }

}
