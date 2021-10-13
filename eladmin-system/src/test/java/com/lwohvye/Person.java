package com.lwohvye;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;

import java.math.BigDecimal;
import java.util.List;

/**
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

    static List<Friend> friends;

//    static ListNode listNode;

    public void copy(Person source) {
        BeanUtil.copyProperties(source, this, CopyOptions.create().setIgnoreNullValue(true));
    }

}
