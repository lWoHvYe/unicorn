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

package com.unicorn.ai.tools.model;

import java.math.BigDecimal;
import java.util.Set;

public record ToolPolicy(String name, boolean write, Set<String> allowedRoles, boolean requireApproval,
                         BigDecimal approvalThreshold) {

    public boolean needsApproval(BigDecimal amount) {
        return requireApproval || (approvalThreshold != null && amount != null && amount.compareTo(approvalThreshold) > 0);
    }
}
