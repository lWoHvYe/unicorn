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
package com.lwohvye.tools.service.impl

import com.lwohvye.core.utils.FileUtils
import com.lwohvye.tools.config.AwsCOSProperties
import com.lwohvye.tools.service.ICOSService
import com.lwohvye.tools.utils.COSUtils
import org.apache.logging.log4j.LogManager
import org.springframework.stereotype.Service
import org.springframework.web.multipart.MultipartFile
import software.amazon.awssdk.transfer.s3.S3ClientConfiguration

/**
 * @date 2021年09月05日 16:22
 */
@Service
class COSServiceImpl(val s3ClientConfiguration: S3ClientConfiguration, val cosProperties: AwsCOSProperties) :
    ICOSService {
    companion object {
        private val log = LogManager.getLogger()
    }

    override fun upload(file: MultipartFile, cosPath: String?) {
        val sampleFile = FileUtils.toFile(file)
        val updateStatus =
            COSUtils.fileUpload(s3ClientConfiguration, cosProperties.bucketName, sampleFile.toPath(), cosPath)
        log.info("上传结果：{} ", updateStatus)
        log.info("源文件名称：{} || 文件地址：{} ", file.originalFilename, cosPath)
    }

    override fun download(storePath: String?, cosPath: String?) {
        val downloadStatus = COSUtils.fileDownload(s3ClientConfiguration, cosProperties.bucketName, storePath, cosPath)
        log.info("下载结果: {} ", downloadStatus)
        log.info("下载成功，storePath：{} || cosPath: {}", storePath, cosPath)
    }
}
