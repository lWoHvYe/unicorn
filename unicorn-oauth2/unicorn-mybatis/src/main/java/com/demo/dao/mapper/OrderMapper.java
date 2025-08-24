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

package com.demo.dao.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.demo.dao.entity.Order;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

public interface OrderMapper extends BaseMapper<Order> {

    List<Order> selectDetailsByCondition(@Param("orderId") Long orderId, @Param("customerId") Long customerId,
                                         @Param("orderStatus") Integer orderStatus);

    /*有N+1问题*/
    List<Order> selectDetailsNByCondition(@Param("orderId") Long orderId, @Param("customerId") Long customerId,
                                          @Param("orderStatus") Integer orderStatus);

    // 这里不能自动绑定，不要这么用。当存在多条详情时，会返回重复的订单记录
    @Select("SELECT o.*, od.* FROM orders o LEFT JOIN order_detail od ON o.order_id = od.order_id WHERE o.order_id = #{orderId}")
    List<Order> selectOrderWithDetails(Long orderId);

}
