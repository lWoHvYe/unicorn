/*
 *    Copyright (c) 2025.  lWoHvYe(Hongyan Wang)
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

package com.demo.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.demo.dao.entity.Order;
import com.demo.dao.entity.OrderDetail;
import com.demo.dao.mapper.OrderDetailMapper;
import com.demo.dao.mapper.OrderMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class OrderService {
    @Autowired
    private OrderMapper orderMapper;
    @Autowired
    private OrderDetailMapper orderDetailMapper;

    // 获取订单及详情
    public List<Order> getOrderWithDetails(Long orderId, Long customerId, Integer orderStatus) {
        return orderMapper.selectDetailsByCondition(orderId, customerId, orderStatus);
    }

    /**
     * 这里采用先只查Order，再用in查询相关detail，最终内存过滤的方式，多了次查询，但在单个ordr有很多detail时，可能会好一些，尤其是order需要分页等操作时
     * 当然另一方面就是，若条件主要在order表，想多表join只能写sql了，这里提供了另一种实现方式。但若很多条件在orderDetails表，这种方法不是很好，因为第一步order查询会有很多不相关数据。
     *
     * @date 2025/8/24 08:15
     */
    public List<Order> getOrderWithDetailsN(Long customerId, Integer orderStatus) {
        var queryWrapper = Wrappers.lambdaQuery(Order.class);
        queryWrapper.eq(Order::getCustomerId, customerId);
        queryWrapper.eq(Order::getOrderStatus, orderStatus);
        var orders = orderMapper.selectList(queryWrapper);
        var orderIds = orders.stream().map(Order::getOrderId).collect(Collectors.toSet());
        var orderDetailsMap = orderDetailMapper.selectList(new LambdaQueryWrapper<OrderDetail>().in(OrderDetail::getOrderId, orderIds))
                .stream().collect(Collectors.groupingBy(OrderDetail::getOrderId));
        orders.forEach(order -> {
            order.setOrderDetails(orderDetailsMap.get(order.getOrderId()));
        });
        return orders;
    }

    public List<Order> getOrdersByCustomer(Long customerId) {
        return orderMapper.selectList(new QueryWrapper<Order>().eq("customer_id", customerId));
    }

    public List<Order> getAllOrders() {
        return orderMapper.selectList(new QueryWrapper<>());
    }

    @Transactional(rollbackFor = Exception.class)
    public boolean saveOrderWithDetails(Order order) {
        // 1. 保存订单
        if (orderMapper.insert(order) <= 0) {
            throw new RuntimeException("订单保存失败");
        }

        // 2. 设置订单详情中的订单ID
        Long orderId = order.getOrderId();
        List<OrderDetail> details = order.getOrderDetails();
        if (details != null && !details.isEmpty()) {
            details.forEach(detail -> {
                detail.setOrderId(orderId);
                detail.setOrderNo(order.getOrderNo());
                detail.setDeleted(0); // 数据库默认值未生效，需手动设置（h2）
            });

            // 3. 批量保存订单详情
            if (!saveOrderDetailsBatch(details)) {
                throw new RuntimeException("订单详情保存失败");
            }
        }
        return true;
    }

    private boolean saveOrderDetailsBatch(List<OrderDetail> details) {
        // 使用 MyBatis-Plus 的批量保存（需要配置 SQL 注入器）
        return orderDetailMapper.insertBatchSomeColumn(details) > 0;
    }
}
