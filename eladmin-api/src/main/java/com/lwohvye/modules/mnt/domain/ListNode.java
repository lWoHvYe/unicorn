package com.lwohvye.modules.mnt.domain;

/**
 * @author Hongyan Wang
 * @description 链表实体类
 * @date 2020/12/13 11:18
 */
public class ListNode {
    int val;
    ListNode next;   // 下一个链表对象

    ListNode(int val) {
        this.val = val;
    }  //赋值链表的值

    ListNode(int val, ListNode next) {
        this.val = val;
        this.next = next;
    }
}
