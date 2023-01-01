/*
 *    Copyright (c) 2021-2023.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.tools.service.impl;

import com.lwohvye.core.utils.FileUtils;
import com.lwohvye.tools.config.AwsCOSProperties;
import com.lwohvye.tools.service.ICOSService;
import com.lwohvye.tools.utils.COSUtils;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import software.amazon.awssdk.transfer.s3.S3ClientConfiguration;

/**
 * @date 2021年09月05日 16:22
 */
@Slf4j
@Service
public class COSServiceImpl implements ICOSService {

    // 引入starter后，可直接注入
    @Autowired
    private S3ClientConfiguration s3ClientConfiguration;

    @Autowired
    private AwsCOSProperties cosProperties;

    @SneakyThrows
    @Override
    public void upload(MultipartFile file, String cosPath) {
        var sampleFile = FileUtils.toFile(file);
        var updateStatus = COSUtils.fileUpload(s3ClientConfiguration, cosProperties.getBucketName(), sampleFile.toPath(), cosPath);
        log.info("上传结果：{} ", updateStatus);
        log.info("源文件名称：{} || 文件地址：{} ", file.getOriginalFilename(), cosPath);
    }

    @SneakyThrows
    @Override
    public void download(String storePath, String cosPath) {
        var downloadStatus = COSUtils.fileDownload(s3ClientConfiguration, cosProperties.getBucketName(), storePath, cosPath);
        log.info("下载结果: {} ", downloadStatus);
        log.info("下载成功，storePath：{} || cosPath: {}", storePath, cosPath);
    }
}
