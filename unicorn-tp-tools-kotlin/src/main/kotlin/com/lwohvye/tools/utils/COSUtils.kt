/*
 *    Copyright (c) 2024.  lWoHvYe(Hongyan Wang)
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

//snippet-sourcedescription:[FileUpDownOps.java demonstrates how to upload and download an object to an Amazon Simple Storage Service (Amazon S3) bucket.]
//snippet-keyword:[AWS SDK for Java v2]
//snippet-service:[Amazon S3]
/*
   Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
   SPDX-License-Identifier: Apache-2.0
*/
package com.lwohvye.tools.utils

import software.amazon.awssdk.services.s3.model.GetObjectRequest
import software.amazon.awssdk.services.s3.model.PutObjectRequest
import software.amazon.awssdk.transfer.s3.DownloadFileRequest
import software.amazon.awssdk.transfer.s3.S3ClientConfiguration
import software.amazon.awssdk.transfer.s3.S3TransferManager
import software.amazon.awssdk.transfer.s3.UploadFileRequest
import java.nio.file.Path
import java.nio.file.Paths

// snippet-start:[s3.tm.java2.file_up_down_ops.import]
// snippet-end:[s3.tm.java2.file_up_down_ops.import]
object COSUtils {
    fun fileUpload(
        clientConfiguration: S3ClientConfiguration?,
        bucketName: String?,
        originPath: Path?,
        cosPath: String?
    ): String {
        initTransferManager(clientConfiguration).use { transferManager ->
            val fileUpload = transferManager.uploadFile { uploadFileRequestBuilder: UploadFileRequest.Builder ->
                uploadFileRequestBuilder
                    .putObjectRequest { putObjectRequestBuilder: PutObjectRequest.Builder ->
                        putObjectRequestBuilder.bucket(
                            bucketName
                        ).key(cosPath)
                    }
                    .source(originPath)
            }
            val uploadResult = fileUpload.completionFuture().join()
            return uploadResult.response().eTag()
        }
    }

    fun fileDownload(
        clientConfiguration: S3ClientConfiguration?,
        bucketName: String?,
        storePath: String?,
        cosPath: String?
    ): String {
        initTransferManager(clientConfiguration).use { transferManager ->
            val downloadFile = transferManager.downloadFile { downloadFileRequestBuilder: DownloadFileRequest.Builder ->
                downloadFileRequestBuilder
                    .getObjectRequest { getObjectRequestBuilder: GetObjectRequest.Builder ->
                        getObjectRequestBuilder.bucket(
                            bucketName
                        ).key(cosPath)
                    }
                    .destination(storePath?.let { Paths.get(it) })
            }
            val downloadResult = downloadFile.completionFuture().join()
            return downloadResult.response().eTag()
        }
    }

    private fun initTransferManager(clientConfiguration: S3ClientConfiguration?): S3TransferManager {
        return S3TransferManager.builder()
            .s3ClientConfiguration(clientConfiguration)
            .build()
    }
}
