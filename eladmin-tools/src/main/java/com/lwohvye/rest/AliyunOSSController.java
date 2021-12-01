/*
 *    Copyright (c) 2021.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.rest;

import com.lwohvye.annotation.rest.AnonymousGetMapping;
import com.lwohvye.annotation.rest.AnonymousPostMapping;
import com.lwohvye.service.IAliyunOSSService;
import com.lwohvye.utils.result.ResultInfo;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author Hongyan Wang
 * @date 2021年09月05日 18:10
 */
@Slf4j
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/aliyunOSS")
@Api(tags = "工具：对象存储OSS")
public class AliyunOSSController {

    private final IAliyunOSSService aliyunOSSService;

    @AnonymousPostMapping
    @ApiOperation("分片上传")
    public ResponseEntity<Object> multipartUpload(MultipartFile file) {
        aliyunOSSService.multipartUploadFile(file);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }

    @AnonymousGetMapping
    @ApiOperation("断点续传下载")
    public ResponseEntity<Object> downloadFile(String ossUri, String downloadPath) {
        aliyunOSSService.downloadFile(ossUri, downloadPath);
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.OK);
    }

}
