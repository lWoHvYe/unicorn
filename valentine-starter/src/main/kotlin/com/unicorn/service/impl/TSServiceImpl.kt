/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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
package com.unicorn.service.impl

import com.unicorn.service.ITSService
import org.apache.logging.log4j.LogManager
import org.springframework.context.annotation.Scope
import org.springframework.context.annotation.ScopedProxyMode
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.stereotype.Service
import org.springframework.util.StringUtils
import org.springframework.web.context.WebApplicationContext

@Service // 解决Bean链中某个Bean需要多例的问题
// 使用session和request产生了一个新问题，生成controller的时候需要service作为controller的成员，但是service只在收到请求（可能是request也可能是session）时才会被实例化，controller拿不到service实例。
// 为了解决这个问题，@Scope注解添加了一个proxyMode的属性，有两个值ScopedProxyMode.INTERFACES和ScopedProxyMode.TARGET_CLASS，前一个表示表示Service是一个接口使用JDK动态代理模式，后一个表示Service是一个类使用CGLIB代理模式。
// 比较有趣的是，这里用JDK或者CGLIB都可以，理论上虽然是实现了接口，注入的也是接口，但@Service是标记在类上的，所以一般/默认都是使用的CGLIB，但这里用JDK也能正常代理（且确实使用的JDK动态代理）
@Scope(value = WebApplicationContext.SCOPE_REQUEST, proxyMode = ScopedProxyMode.INTERFACES)
class TSServiceImpl(val db: JdbcTemplate) : ITSService {
    private var tsName: String? = null
    override fun setField(tsName: String?) {
        if (StringUtils.hasText(tsName)) this.tsName = tsName
    }

    override fun outIn(): String? {
        log.info("ThreadName -> ${Thread.currentThread().name} insField -> $tsName dbField ${db.isLazyInit} ")
        return tsName
    }

    companion object {
        private val log = LogManager.getLogger()
    }
}
