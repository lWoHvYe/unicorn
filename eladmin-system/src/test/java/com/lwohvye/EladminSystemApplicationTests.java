package com.lwohvye;

import lombok.val;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.HashMap;
import java.util.Objects;

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

        var m = new HashMap<String, Integer>();
        m.put("H", m.getOrDefault("H", 1) + 1); // 获取散列集中H的值，并加1
        m.merge("H", 1, Integer::sum); // 如果key与一个非null值v关联,将函数应用到v和value,将key与结果关联,如果结果为null,则删除这个键。否则，将key与函数的结果关联，返回get(key)
        m.compute("H", (k, v) -> Objects.nonNull(v) ? k.hashCode() + v : k.hashCode()); // 将函数应用到key和get(key),将key与结果关联,如果结果为 null,则删除这个键。返回get(key)

    }

//    Person person;

}

