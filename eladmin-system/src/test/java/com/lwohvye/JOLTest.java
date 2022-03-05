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

package com.lwohvye;

import org.junit.jupiter.api.Test;
import org.openjdk.jol.info.ClassLayout;
import org.openjdk.jol.info.GraphLayout;

public class JOLTest {

    // 直接运行报错，Cannot get the field offset, try with -Djol.magicFieldOffset=true ,需在VM中加上
    @Test
    public void test01() {
        var friend = new Friend("在一起", 8);
        var person = new Person(1L, "咸鱼", 1, 12, true, 18.0F, friend, new ListNode(10));
        System.out.println(ClassLayout.parseInstance(person).toPrintable()); // 查看对象内部信息 // 1
        synchronized (person) {
            System.out.println(ClassLayout.parseInstance(person).toPrintable()); // 2
        }
        try {
            Thread.sleep(5000);
            // 在未开启偏向锁时，前后两次无锁（1和3）的person，输出是一样的。
            // 通过 -XX:+UseBiasedLocking 开启偏向锁后，若无2，则1和3输出依旧一样（1.8也许睡5秒就自动开启偏向锁了），当有2的话，2和3的输出会一样，应该是加上了偏向锁
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println(ClassLayout.parseInstance(person).toPrintable()); // 3

        System.out.println(GraphLayout.parseInstance(person).toPrintable()); // 查看对象外部信息，包含引用对象
    }
}
