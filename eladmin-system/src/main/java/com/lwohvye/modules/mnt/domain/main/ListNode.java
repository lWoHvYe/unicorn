package com.lwohvye.modules.mnt.domain.main;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * @author Hongyan Wang
 * @description 链表实体类
 * @date 2020/12/13 11:18
 */
@Getter
@Setter
@NoArgsConstructor
public class ListNode {
    int val;
    ListNode next;   // 下一个链表对象

    ListNode(int x) {
        val = x;
    }  //赋值链表的值
}
