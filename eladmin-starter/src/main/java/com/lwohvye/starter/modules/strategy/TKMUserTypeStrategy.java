/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.starter.modules.strategy;

import com.lwohvye.sys.modules.system.annotation.UserTypeHandlerAnno;
import com.lwohvye.sys.modules.system.strategy.ExtraUserTypeStrategy;
import java.util.Collections;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@UserTypeHandlerAnno(typeName = "FOUR")
public class TKMUserTypeStrategy extends ExtraUserTypeStrategy {
    @Override
    public List<GrantedAuthority> grantedAuth(Long userId) {
        return Collections.emptyList();
    }
}
