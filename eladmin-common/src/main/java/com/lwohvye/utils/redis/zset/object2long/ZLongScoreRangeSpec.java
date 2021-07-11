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
package com.lwohvye.utils.redis.zset.object2long;

/**
 * {@link Object2LongZSet}中“score”范围描述信息 - specification模式
 */
public final class ZLongScoreRangeSpec {
    /**
     * 最低分数
     */
    final long min;
    /**
     * 是否去除下限
     * exclusive
     */
    final boolean minex;
    /**
     * 最高分数
     */
    final long max;
    /**
     * 是否去除上限
     * exclusive
     */
    final boolean maxex;

    public ZLongScoreRangeSpec(long min, boolean minex, long max, boolean maxex) {
        this.min = min;
        this.max = max;
        this.minex = minex;
        this.maxex = maxex;
    }
}
