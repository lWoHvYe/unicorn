/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.vs.rest;

import com.lwohvye.core.annotation.RespResultBody;
import com.lwohvye.core.annotation.rest.AnonymousGetMapping;
import com.lwohvye.core.utils.result.ResultInfo;
import com.lwohvye.sys.modules.infrastructure.constants.LogRecordType;
import com.lwohvye.sys.common.annotation.ApiVersion;
import com.mzt.logapi.starter.annotation.LogRecord;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class RsLogController {

    @LogRecord(
            fail = "执行失败，失败原因：「{{#_errorMsg}}」",
            success = "收到请求{{#version}},执行结果:{{#_ret}}",
            type = LogRecordType.PORTAL, bizNo = "20220920"
    )
    @RespResultBody
    @ApiVersion(3) // 指定从v3开始
    @AnonymousGetMapping(value = {"/rs/valentine/{version}/p2p", "/rs/valentine/{version}/default"}) // @RequestMapping的path是支持多个的
    public ResultInfo<String> indexVersion(@PathVariable String version) {
        return ResultInfo.success(String.format("Version %s Backend service started successfully", version));
    }
}
