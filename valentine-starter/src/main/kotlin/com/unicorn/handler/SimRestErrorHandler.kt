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
package com.unicorn.handler

import org.springframework.http.client.ClientHttpResponse
import org.springframework.web.client.DefaultResponseErrorHandler

/**
 * RestTemplate异常处理，可以实现ResponseErrorHandler，也可以继承DefaultResponseErrorHandler
 *
 * @date 2022/6/11 8:12 PM
 */
class SimRestErrorHandler : DefaultResponseErrorHandler() {
    override fun handleError(response: ClientHttpResponse) {
        // 这样空复写可以做到，从 Response 获取 HttpStatus 和 body 中的报错信息 而不抛出异常
    }
}
