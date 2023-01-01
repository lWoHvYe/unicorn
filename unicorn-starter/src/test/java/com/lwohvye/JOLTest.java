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

package com.lwohvye;

import org.junit.jupiter.api.Test;
import org.openjdk.jol.info.ClassLayout;
import org.openjdk.jol.info.GraphLayout;

public class JOLTest {

    // 直接运行报错，Cannot get the field offset, try with -Djol.magicFieldOffset=true ,需在VM中加上
    // 总结：初始为biasable，首次加锁后biased。
    // 若调用wait()，则膨胀为 fat lock，之后解锁、调用notify()等，仍为fat lock（即锁不会降级）
    // 若只调用notify()，则升级为thin lock，解锁后变为non-biasable
    // 在偏向锁的状态下，发生锁的竞争时，会做偏向锁撤销，并膨胀成重量锁 fat lock
    // 在偏向锁的状态下，未发生锁的竞争时，当另一线程试图获取锁时（此时上一线程已解锁），则升级为thin lock，解锁后变为non-biasable，当未出现锁竞争时，加解锁一直在轻量与无锁间切换（同样不会降级）。
    @Test
    public void test01() {
        var friend = new Friend("在一起", 8);
        var person = new Person(1L, "咸鱼", 1, 12, true, 18.0F, friend, new ListNode(10));
        System.out.println("初始：" + ClassLayout.parseInstance(person).toPrintable()); // 查看对象内部信息 // 1
        new Thread(() -> {
            // 在这个逻辑里，子线程会先等待主线程拿到锁后，再进行些事情，在子线程醒来后，就已经是 fat lock了，是wait()导致的
            // 通过注释掉wait()和notify()的逻辑，可以验证，在偏向状态，发生锁的争抢时，会膨胀成重量锁
            // 在注释掉wait()和notify()之后，通过调整sleep的时间，可以验证，在偏向状态，当未发生锁的争抢（获取时，上一个已释放锁），另一线程加锁时，状态为thin lock，且解锁后变为 non-biasable
            System.out.println("子线程，初始" + ClassLayout.parseInstance(person).toPrintable());
            try {
                Thread.sleep(5000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            System.out.println("子线程，加锁前" + ClassLayout.parseInstance(person).toPrintable());
            synchronized (person) {
                System.out.println("子线程，加锁后" + ClassLayout.parseInstance(person).toPrintable());
                // person.notify();
                // System.out.println("子线程，notify后" + ClassLayout.parseInstance(person).toPrintable());
            }

            System.out.println("子线程，解锁后" + ClassLayout.parseInstance(person).toPrintable());
        }).start();

        System.out.println("加锁前" + ClassLayout.parseInstance(person).toPrintable());
        synchronized (person) {
            System.out.println("加锁后" + ClassLayout.parseInstance(person).toPrintable()); // 2
            try {
                Thread.sleep(2000);
                // Thread.sleep(6000);
                // 理论：当调用一个锁对象的wait或notify方法时，若当前锁的状态是偏向锁或轻量级锁则会先膨胀成重量级锁。
                // person.wait(); // 当开启此处（最初为超时等待），注释notify()，各处状态：1(biasable 偏向模式)  2(biased 偏向锁)  4(fat lock 重量锁)  3(fat lock)
                // person.notify(); // 当开启此处，注释wait时，各处状态：1(biasable 偏向模式)  2(biased 偏向锁)  4(thin lock 轻量锁)   3(non-biasable 无锁状态)
                // System.out.println("wait之后（被notify）" + ClassLayout.parseInstance(person).toPrintable()); // 4
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        try {
            Thread.sleep(5000);
            // 在未开启偏向锁时，前后两次无锁（1和3）的person，输出是一样的。
            // 通过 -XX:+UseBiasedLocking 开启偏向锁后，若无2，则1和3输出依旧一样（1.8也许睡5秒就自动开启偏向锁了），当有2的话，2和3的输出会一样，应该是加上了偏向锁
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println("解锁后" + ClassLayout.parseInstance(person).toPrintable()); // 3

        System.out.println(GraphLayout.parseInstance(person).toPrintable()); // 查看对象外部信息，包含引用对象
    }
}
