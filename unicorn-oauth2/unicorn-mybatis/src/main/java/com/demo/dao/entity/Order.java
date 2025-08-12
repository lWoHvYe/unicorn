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

package com.demo.dao.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

@Data
@TableName("orders")
public class Order {

    @TableId(type = IdType.AUTO)
    private Long orderId;
    private String orderNo;
    private Long customerId;
    private Integer orderStatus;
    private BigDecimal totalAmount;
    private BigDecimal paymentAmount;
    private BigDecimal discountAmount;
    private Integer paymentMethod;
    private Date paymentTime;
    private String shippingAddress;
    private String shippingName;
    private String shippingPhone;
    private String shippingCode;
    private String shippingCompany;
    private Date shippingTime;
    private Date completeTime;
    private String orderRemark;
    private Date createdTime;
    private Date updatedTime;

    @TableField(exist = false)
    private List<OrderDetail> orderDetails;
}
