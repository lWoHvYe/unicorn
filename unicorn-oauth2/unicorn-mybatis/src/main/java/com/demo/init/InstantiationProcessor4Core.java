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
import com.demo.dao.entity.User;
import com.demo.dao.mapper.UserMapper;
import com.demo.service.OrderService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.stereotype.Component;

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
            orderService.getOrderWithDetails(null, 1001L, null).forEach(System.out::println);

        }
    }
}
