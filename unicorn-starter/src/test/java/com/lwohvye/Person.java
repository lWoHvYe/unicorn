/*
 *    Copyright (c) 2021-2023.  lWoHvYe(Hongyan Wang)
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

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;

import java.math.BigDecimal;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.*;

/**
 * Record类，本身属性是private final的，内部允许有其他属性，这些需要是static的(Instance field is not allowed in record)
 *
 * @author Hongyan Wang
 * @date 2021年04月10日 10:10
 */
public record Person(Long id, String name, Integer sex, Integer age, Boolean isMember, Float point, Friend friend, ListNode listNode) {
    /*
    java 16默认情况下强封装JDK内部构件。当前不少基于反射的方法都出现了问题，比如lombok，而record的使用场景尚未知，在这个还是8为主的时代，推广需要大量的时间。
    record推出背后的目标是使开发人员能够将相关字段作为单个不可变数据项组合在一起，而不需要编写冗长的代码。
    这意味着，每当您想要向您的记录添加更多的字段/方法时，请考虑是否应该使用完整的类来代替它。
     */
    static Double d;

    static BigDecimal bigDecimal;

    static Set<Friend> friends = new HashSet<>();

    private static final Random RANDOM;

    static {
        try {
            RANDOM = SecureRandom.getInstanceStrong();
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
    }

//    static ListNode listNode;

    /**
     * Record类的构造是这样的，与传统的类是不一样的
     *
     * @param id       /
     * @param name     /
     * @param sex      /
     * @param age      /
     * @param isMember /
     * @param point    /
     * @param friend   /
     * @param listNode /
     * @date 2022/5/2 12:55 PM
     */
    public Person {
        age >>= 1;
        point *= 2;
        if (Objects.isNull(d)) d = Double.valueOf(point);
        friends.add(friend);
    }

    // A record class can declare instance methods.
    public void copy(Person source) {
        BeanUtil.copyProperties(source, this, CopyOptions.create().setIgnoreNullValue(true));
    }

    // A record class can declare static methods, fields, and initializers.
    public static int joy() {
        return RANDOM.nextInt();
    }

    private String doJoy() {
        return name + isMember();
    }

    private String haveJoy(Integer i) {
        return String.format("Integer %s or %s and %s", name, i, isMember);
    }

    private String haveJoy(Number i) {
        return String.format("Number %s or %s and %s", name, i, isMember);
    }

    private String haveJoy(Integer i, String msg) {
        return String.format("Integer + msg %s or %s and %s | %s", name, i, msg, isMember);
    }

    private Object haveJoy(Long i, String msg) {
        return String.format("Long + msg %s or %s and %s | %s", name, i, msg, isMember);
    }
}
