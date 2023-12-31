/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.sys.modules.system.service.local;

import com.lwohvye.sys.modules.system.service.ITerminalService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class WindowsTerminalServiceImpl implements ITerminalService {
    @Override
    public void prSysName() {
        log.warn(" This is Windows 11 ");
    }
}
