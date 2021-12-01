/*
 *    Copyright (c) 2021.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.modules.system.handler;

import com.lwohvye.base.BaseService;
import org.springframework.security.core.GrantedAuthority;

import java.util.List;

/**
 * @author Hongyan Wang
 * @description 业务处理基类，密封类sealed。需通过permits指定子类。子类可以是final标记的实现类、sealed标记的密封类、non-sealed标记的非密封类
 * @date 2021年11月02日 16:42
 */
public sealed interface AUserTypeHandler extends BaseService permits AdminUserTypeHandler, NormalUserTypeHandler, DevUserTypeHandler {

    List<GrantedAuthority> handler(Long userId);

}
