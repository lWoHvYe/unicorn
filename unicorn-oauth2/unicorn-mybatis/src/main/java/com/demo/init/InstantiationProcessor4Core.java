/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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
package com.demo.init;

import com.baomidou.mybatisplus.core.toolkit.Assert;
import com.demo.dao.entity.Order;
import com.demo.dao.entity.OrderDetail;
import com.demo.dao.entity.User;
import com.demo.dao.mapper.UserMapper;
import com.demo.service.OrderService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

@Slf4j
@Component
public class InstantiationProcessor4Core implements ApplicationListener<ContextRefreshedEvent> {
    @Override
    public void onApplicationEvent(ContextRefreshedEvent event) {
        if (event.getApplicationContext().getParent() == null) {
            var userMapper = event.getApplicationContext().getBean(UserMapper.class);
            List<User> userList = userMapper.selectList(null);
            Assert.isTrue(5 == userList.size(), "");
            userList.forEach(System.out::println);

            var orderService = event.getApplicationContext().getBean(OrderService.class);

            List<Order> orders = orderService.getOrdersByCustomer(1001L);
            orders.forEach(System.out::println);

            System.out.println("----------");
            orderService.getOrderWithDetails(1L, 1001L, null).forEach(System.out::println);

            System.out.println("----------");

            var order = genNewOrder();

            orderService.saveOrderWithDetails(order);

            System.out.println("插入完成，开始查询");

            orderService.getOrderWithDetailsN(List.of("1001_1", "1001_2")).forEach(System.out::println);
        }
    }

    private static Order genNewOrder() {
        Order order = new Order();
        order.setOrderNo("ORD20230005");
        order.setCustomerId(1001L);
        order.setOrderStatus(1);
        order.setTotalAmount(new BigDecimal("199.99"));
        order.setPaymentAmount(new BigDecimal("199.99"));

        List<OrderDetail> details = new ArrayList<>();
        OrderDetail detail1 = new OrderDetail();
        detail1.setProductId(1001L);
        detail1.setProductName("iphone");
        detail1.setQuantity(2);
        detail1.setProductPrice(new BigDecimal("90.00"));
        detail1.setSubtotalAmount(new BigDecimal("180.00"));
        detail1.setActualAmount(new BigDecimal("99.99"));

        OrderDetail detail2 = new OrderDetail();
        detail2.setProductId(1002L);
        detail2.setProductName("HUAWEI");
        detail2.setQuantity(1);
        detail2.setProductPrice(new BigDecimal("100.00"));
        detail2.setSubtotalAmount(new BigDecimal("100.00"));
        detail2.setActualAmount(new BigDecimal("100.00"));

        details.add(detail1);
        details.add(detail2);
        order.setOrderDetails(details);
        return order;
    }
}
