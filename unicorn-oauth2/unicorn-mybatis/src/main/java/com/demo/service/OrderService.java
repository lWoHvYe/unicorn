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

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.demo.dao.entity.Order;
import com.demo.dao.mapper.OrderMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class OrderService {
    @Autowired
    private OrderMapper orderMapper;

    // 获取订单及详情
    public List<Order> getOrderWithDetails(Long orderId, Long customerId, Integer orderStatus) {
        return orderMapper.selectDetailsByCondition(orderId, customerId, orderStatus);
    }

    public List<Order> getOrdersByCustomer(Long customerId) {
        return orderMapper.selectList(new QueryWrapper<Order>().eq("customer_id", customerId));
    }

    public List<Order> getAllOrders() {
        return orderMapper.selectList(new QueryWrapper<>());
    }
}
