/*
 *    Copyright (c) 2023-2024.  lWoHvYe(Hongyan Wang)
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

package sample.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.oauth2.core.oidc.OidcUserInfo;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import sample.domain.CustomizeUser;
import sample.repo.CustomizeUserInfoRepository;

import java.util.stream.Collectors;

/**
 * Example service to perform lookup of user info for customizing an {@code id_token}.
 */
@Service
public class OidcUserInfoService {

    @Autowired
    private CustomizeUserInfoRepository customizeUserInfoRepository;

    @Transactional(rollbackFor = Exception.class)
    public OidcUserInfo loadUser(String username) {
        var user = customizeUserInfoRepository.findByUsername(username).orElseGet(CustomizeUser::new);
        return OidcUserInfo.builder()
                .subject(username)
                .name(username)
                .nickname(user.getNickName())
                .gender(user.getGender())
                .email(user.getEmail())
                .phoneNumber(user.getPhone())
                // 如果传List : OpenID Connect 1.0 UserInfo Error:
                // The class with java.util.ImmutableCollections$ListN and name of java.util.ImmutableCollections$ListN is not in the allowlist.
                // If you believe this class is safe to deserialize, please provide an explicit mapping using Jackson annotations or by providing a Mixin.
                // If the serialization is only done by a trusted source, you can also enable default typing. See https://github.com/spring-projects/spring-security/issues/4370 for details
                // 有相关的限制，这里的value不能是Long类型，String可以，但实际中，不建议使用userId，而更应用subject/username这些
                .claim("userId", String.valueOf(user.getId()))
                .build();
    }
}

