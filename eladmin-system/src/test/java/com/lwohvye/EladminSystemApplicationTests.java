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

import lombok.val;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.LongAdder;

@ExtendWith(SpringExtension.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class EladminSystemApplicationTests {

    @Test
    public void contextLoads() {
    }

    public static void main(String[] args) {
        final val friend = new Friend("在一起", 8);
        val person = new Person(1L, "咸鱼", 1, 12, true, 18.0F, friend, new ListNode(10));
        final val name = person.name();
        final val age = person.age();
//        隐式的继承自Record类。有equals hashCode toString方法
        final val equals = person.equals(person);
        final val string = person.toString();

        final val next = person.listNode().next;
        final val friends = Person.friends;

        var m = new HashMap<String, Long>();

        var cm0 = new ConcurrentHashMap<String, Long>();
        cm0.put("H", 0L); // 里面要有H，且与oldVal一致。不然下面的replace永远走不出去
        var cm1 = new ConcurrentHashMap<String, LongAdder>();
        var cm2 = new ConcurrentHashMap<String, Long>();
        var cm3 = new ConcurrentHashMap<String, Long>();
        for (int i = 0; i < 3; i++) {
            m.put("H", m.getOrDefault("H", 0L) + 1); // 获取散列集中H的值，并加1

            Long oldVal;
            long newVal;
            do {
                oldVal = cm0.get("H");
                newVal = oldVal == null ? 1 : oldVal + 1;
            } while (!cm0.replace("H", oldVal, newVal)); // 用replace来循环实现。

            // LongAdder可以支持并发环境
            cm1.putIfAbsent("H", new LongAdder()); // 需要先调用这个
            cm1.get("H").increment(); // 不存在时再设置

            // 下面这俩compute和merge是原子操作
            cm2.compute("H", (k, v) -> Objects.nonNull(v) ? 1 + v : 1); // 将函数应用到key和get(key),将key与结果关联,如果结果为 null,则删除这个键。返回get(key)

            cm3.merge("H", 1L, Long::sum); // 如果key与一个非null值v关联,将函数应用到v和value,将key与结果关联,如果结果为null,则删除这个键。否则，将key与函数的结果关联，返回get(key)
        }


        Set<String> words = ConcurrentHashMap.newKeySet(); // 并发集视图。从cm而来，key为并发集，val为Boolean.TRUE
        Set<String> words2 = new ConcurrentHashMap<String, Long>().keySet(1L); // 另一种并发集视图，key为并发集，val为1L

        List<String> syncArrayList = Collections.synchronizedList(new ArrayList<>()); // 对于经常被修改的数组列表， 同步的 ArrayList 可以胜过 CopyOnWriteArrayList()；
        Map<String, Long> syncHashMap = Collections.synchronizedMap(new HashMap<>()); // 对于映射，ConcurrentHashMap较同步的HashMap要好一些。
    }

//    Person person;

}

