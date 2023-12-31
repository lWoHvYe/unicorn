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
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import sample.domain.CustomizeUser;
import sample.repo.CustomizeUserInfoRepository;

@Service
public class CustomizeUserDetailsService implements UserDetailsService {


    @Autowired
    private CustomizeUserInfoRepository customizeUserInfoRepository;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        var customizeUser = customizeUserInfoRepository.findByUsername(username).orElseGet(CustomizeUser::new);

        return User.builder()
                .username(customizeUser.getUsername())
                .password(customizeUser.getPassword())
                .roles(customizeUser.getCustomizeRoles().stream()
                        .map(customizeRole -> customizeRole.getCode().toUpperCase())
                        .toArray(String[]::new))
                .disabled(!customizeUser.getEnabled())
                .build();
    }
}
