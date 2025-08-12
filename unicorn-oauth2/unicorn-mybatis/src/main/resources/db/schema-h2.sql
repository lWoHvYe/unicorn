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

DROP TABLE IF EXISTS `user`;

CREATE TABLE `user`
(
    id    BIGINT NOT NULL COMMENT '主键ID',
    name  VARCHAR(30) NULL DEFAULT NULL COMMENT '姓名',
    age   INT NULL DEFAULT NULL COMMENT '年龄',
    email VARCHAR(50) NULL DEFAULT NULL COMMENT '邮箱',
    PRIMARY KEY (id)
);

-- 订单表
DROP TABLE IF EXISTS orders;
CREATE TABLE orders
(
    order_id         BIGINT PRIMARY KEY AUTO_INCREMENT,
    order_no         VARCHAR(32)    NOT NULL,
    customer_id      BIGINT         NOT NULL,
    order_status     TINYINT        NOT NULL DEFAULT 0,
    total_amount     DECIMAL(12, 2) NOT NULL,
    payment_amount   DECIMAL(12, 2) NOT NULL,
    discount_amount  DECIMAL(12, 2)          DEFAULT 0.00,
    payment_method   TINYINT,
    payment_time     DATETIME,
    shipping_address VARCHAR(255),
    shipping_name    VARCHAR(50),
    shipping_phone   VARCHAR(20),
    shipping_code    VARCHAR(50),
    shipping_company VARCHAR(50),
    shipping_time    DATETIME,
    complete_time    DATETIME,
    order_remark     VARCHAR(500),
    created_time     DATETIME       NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_time     DATETIME       NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

-- 订单详情表
DROP TABLE IF EXISTS order_detail;
CREATE TABLE order_detail
(
    detail_id       BIGINT PRIMARY KEY AUTO_INCREMENT,
    order_id        BIGINT         NOT NULL,
    order_no        VARCHAR(32)    NOT NULL,
    product_id      BIGINT         NOT NULL,
    product_name    VARCHAR(100)   NOT NULL,
    product_image   VARCHAR(255),
    product_price   DECIMAL(12, 2) NOT NULL,
    quantity        INT            NOT NULL,
    subtotal_amount DECIMAL(12, 2) NOT NULL,
    discount_amount DECIMAL(12, 2)          DEFAULT 0.00,
    actual_amount   DECIMAL(12, 2) NOT NULL,
    product_spec    VARCHAR(255),
    refund_status   TINYINT                 DEFAULT 0,
    refund_amount   DECIMAL(12, 2)          DEFAULT 0.00,
    refund_time     DATETIME,
    refund_reason   VARCHAR(255),
    created_time    DATETIME       NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_time    DATETIME       NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

-- 创建索引
CREATE INDEX idx_order_customer ON orders (customer_id);
CREATE INDEX idx_order_status ON orders (order_status);
CREATE INDEX idx_detail_order ON order_detail (order_id);
