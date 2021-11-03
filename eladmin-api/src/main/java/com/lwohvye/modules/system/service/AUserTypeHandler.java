/*
 *  Copyright 2020-2022 lWoHvYe
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.lwohvye.modules.system.service;

import com.lwohvye.base.BaseService;
import org.springframework.security.core.GrantedAuthority;

import java.util.List;

/**
 * @author Hongyan Wang
 * @description 业务处理基类，这里使用抽象类，是为了保证子类的单继承
 * @date 2021年11月02日 16:42
 */
public abstract class AUserTypeHandler implements BaseService {

    public abstract List<GrantedAuthority> handler(Long userId);

}
