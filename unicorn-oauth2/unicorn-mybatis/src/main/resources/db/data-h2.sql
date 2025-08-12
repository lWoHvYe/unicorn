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

DELETE
FROM `user`;

INSERT INTO `user` (id, name, age, email)
VALUES (1, 'Jone', 18, 'test1@baomidou.com'),
       (2, 'Jack', 20, 'test2@baomidou.com'),
       (3, 'Tom', 28, 'test3@baomidou.com'),
       (4, 'Sandy', 21, 'test4@baomidou.com'),
       (5, 'Billie', 24, 'test5@baomidou.com');

-- 插入订单数据
INSERT INTO orders (order_id, order_no, customer_id, order_status, total_amount, payment_amount, created_time)
VALUES (1, 'ORD20230001', 1001, 1, 4298.00, 4298.00, '2023-01-15 10:00:00'),
       (2, 'ORD20230002', 1001, 2, 5998.00, 5598.00, '2023-01-16 14:30:00'),
       (3, 'ORD20230003', 1002, 3, 1299.00, 1199.00, '2023-01-17 09:15:00');

-- 插入订单详情数据
INSERT INTO order_detail (detail_id, order_id, order_no, product_id, product_name, product_price, quantity,
                          subtotal_amount, actual_amount)
VALUES (1, 1, 'ORD20230001', 2001, '智能手机X', 2999.00, 1, 2999.00, 2999.00),
       (2, 2, 'ORD20230002', 2001, '智能手机X', 2999.00, 2, 5998.00, 5598.00),
       (3, 1, 'ORD20230001', 2002, '无线耳机Pro', 1299.00, 1, 1299.00, 1299.00),
       (4, 3, 'ORD20230003', 2002, '无线耳机Pro', 1299.00, 1, 1299.00, 1199.00);
