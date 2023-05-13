/*
 *    Copyright (c) 2022-2023.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.designpatterns.state;

import lombok.Data;

/**
 * 任务有多个状态，在任一种状态，基于不同的条件，可流转到不同的状态。若不引入外部流程引擎，可基于状态模式进行相关设计
 *
 * @date 2022/3/13 7:13 PM
 */
@Data
class Task {
    private Long taskId;
    // 初始化为初始态
    private State state = new TaskInit();

    // 更新状态
    public void updateState(ActionType actionType) {
        state.update(this, actionType);
    }
}
