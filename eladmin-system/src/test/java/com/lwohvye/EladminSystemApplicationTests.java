package com.lwohvye;

import lombok.val;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

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
    }

//    Person person;

}

