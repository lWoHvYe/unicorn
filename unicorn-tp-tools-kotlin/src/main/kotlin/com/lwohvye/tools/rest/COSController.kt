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
package com.lwohvye.tools.rest

import com.lwohvye.core.utils.result.ResultInfo
import com.lwohvye.tools.service.ICOSService
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.tags.Tag
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController
import org.springframework.web.multipart.MultipartFile

/**
 * @author Hongyan Wang
 * @date 2021年09月05日 18:10
 */
@RestController
@RequestMapping("/api/awsCOS")
@Tag(name = "AWSCOSController", description = "工具：对象存储COS")
class COSController(val cosService: ICOSService) {
    @PostMapping
    @Operation(summary = "上传")
    fun multipartUpload(file: MultipartFile, cosPath: String?): ResultInfo<String> {
        cosService.upload(file, cosPath)
        return ResultInfo.success()
    }

    @GetMapping
    @Operation(summary = "下载")
    fun downloadFile(storePath: String?, cosPath: String?): ResultInfo<String> {
        cosService.download(storePath, cosPath)
        return ResultInfo.success()
    }
}
