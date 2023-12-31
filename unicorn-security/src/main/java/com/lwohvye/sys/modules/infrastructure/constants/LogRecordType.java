/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.sys.modules.infrastructure.constants;


/**
 * @author muzhantong
 * create on 2020/4/30 10:56 上午
 */
public class LogRecordType {

    private LogRecordType() {
        throw new IllegalStateException("Utility class");
    }

    public static final String PORTAL = "Portal";

    public static final String SYS_ADMIN = "SysAdmin";

    public static final String FOUNDATIONAL = "Foundational";

    public static final String FUNCTIONAL = "Functional";
}
