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

package com.lwohvye.designpatterns.decorator;

// 能够对活动做另一种控制的包装类
class DesertControlDecorator extends ActivityDecorator {
    public DesertControlDecorator(ActivityInterface activity) {
        super(activity);
    }

    @Override
    public void participate(Long userId) {
        // 对目标用户做一些操作
        System.out.println("desert");
        // 更新任务状态为进行中
        activity.participate(userId);
    }
}
