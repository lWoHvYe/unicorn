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
package com.unicorn.rest

import com.lwohvye.core.annotation.RespResultBody
import com.lwohvye.core.annotation.log.OprLog
import com.lwohvye.core.annotation.rest.AnonymousGetMapping
import com.lwohvye.core.utils.result.ResultInfo
import com.lwohvye.sys.common.annotation.ApiVersion
import com.lwohvye.sys.modules.infrastructure.constants.LogRecordType
import com.lwohvye.sys.modules.system.strategy.AuthHandlerContext
import com.mzt.logapi.starter.annotation.LogRecord
import jakarta.annotation.PostConstruct
import lombok.extern.slf4j.Slf4j
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.beans.factory.annotation.Value
import org.springframework.context.annotation.Lazy
import org.springframework.context.annotation.Profile
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.PathVariable
import org.springframework.web.bind.annotation.RestController
import java.time.Duration
import java.time.Instant

@Slf4j
@Profile("dev")
@RestController
class RsLogController {
    // #{…} 主要用于加载外部属性文件中的值
    // ${…} 用于执行SpEl表达式，并将内容赋值给属性
    // #{…} 和${…} 可以混合使用，但是必须#{}外面，${}在里面
    @Value("\${local.rs.str}")
    private val simStr: String? = null

    @Value("#{\${local.rs.a-map}}") // properties格式不要带最外层的双引号，而yml格式需要带，这个差别要注意

    private val aMap: Map<String, String>? = null

    @Value("#{'\${local.rs.aList}'.split(',')}") // 根据这个，可以支持不同的分隔符

    private val aList: List<String>? = null

    @Value("\${local.rs.iList}")
    private val iList: List<Int>? = null

    @Value("\${local.rs.iList}")
    private var ints: Array<Int>? = null

    @PostConstruct
    fun who() {
        println(" The module of Cur-Starter is ${javaClass.module}")
    }

    @Lazy
    @Autowired
    private val authHandlerContext: AuthHandlerContext? = null

    /**
     * 访问首页提示
     * 因为自定义了处理逻辑，所以下面这俩的RequestCondition是不一样的，所以能共存，且因为定义了优先新版本，所以在v1时走第一个，[v2+ 走第二个
     * 但注意⚠️：这种path上带param是与定义的匿名访问注解AnonymousAccess不兼容的（不生效）
     *
     * @return /
     * @see com.lwohvye.sys.common.handler.ApiVersionRequestMappingHandlerMapping
     */
    @RespResultBody
    @ApiVersion
    @AnonymousGetMapping("/valentine/v1/p2p", "/valentine/{version}/p2p")
    fun index(@PathVariable(required = false) version: String?): String {
        val startTime = Instant.now()
        val instance = authHandlerContext!!.getInstance(4)
        instance.grantedAuth(2022L)
        return Duration.between(Instant.now(), startTime).toString()
        //        return "Backend service started successfully"
    }

    @RespResultBody
    @ApiVersion(2)
    @AnonymousGetMapping("/valentine/{version}/p2p")
    fun indexCCVer(@PathVariable version: String?): List<String> {
        return java.util.List.of(String.format("CCVer %s Backend service started successfully", version))
    }

    @OprLog("ooo")
    @LogRecord(
        fail = "执行失败，失败原因：「{{#_errorMsg}}」",
        success = "收到请求{{#version}},执行结果:{{#_ret}}",
        type = LogRecordType.PORTAL,
        bizNo = "20220920"
    )
    @RespResultBody
    @ApiVersion(3) // 指定从v3开始
    @AnonymousGetMapping(value = ["/rs/valentine/{version}/p2p", "/rs/valentine/{version}/default"]) // @RequestMapping的path是支持多个的
    @Throws(InterruptedException::class)
    fun indexVersion(@PathVariable version: String?): ResultInfo<String> {
        Thread.sleep(Duration.ofSeconds(1L)) // 使用JMeter  500 * 80，使用VisualVM monitor，只增加十几个thread，整体用时81s，也说明了Fibers/Loom较传统Thread的不同之处，
        // 随着压力及时间的增长，会逐渐有ForkJoinPool-1-worker-xx的Thread被Create（这部分Thread在free后，会wait一段时间再Terminate），不清楚具体的mechanism，已知的是一个OS Thread可以Manage很多的VisualThread
        // 另Heap的Use增加了很多，这应该验证了在VT block时，会将stack放到Heap上
        return ResultInfo.success(String.format("Version %s Backend service started successfully", version))
    }

    /**
     * 匹配采用的最佳适配，当传v4时，会匹配到这个方法
     */
    @RespResultBody
    @AnonymousGetMapping("/valentine/v4/p2p")
    fun indexClVer(): ResponseEntity<ResultInfo<String>> {
        return ResponseEntity(
            ResultInfo.success("ClVersion v4 Backend service started successfully"),
            HttpStatus.CREATED
        )
    }
}
