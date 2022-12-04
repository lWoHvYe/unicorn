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

//snippet-sourcedescription:[FileUpDownOps.java demonstrates how to upload and download an object to an Amazon Simple Storage Service (Amazon S3) bucket.]
//snippet-keyword:[AWS SDK for Java v2]
//snippet-service:[Amazon S3]
/*
   Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
   SPDX-License-Identifier: Apache-2.0
*/
package com.lwohvye.tools.utils;

// snippet-start:[s3.tm.java2.file_up_down_ops.import]

import software.amazon.awssdk.transfer.s3.S3ClientConfiguration;
import software.amazon.awssdk.transfer.s3.S3TransferManager;

import java.nio.file.Path;
import java.nio.file.Paths;
// snippet-end:[s3.tm.java2.file_up_down_ops.import]

public class COSUtils {

    public static String fileUpload(S3ClientConfiguration clientConfiguration, String bucketName, Path originPath, String cosPath) {
        try (var transferManager = initTransferManager(clientConfiguration)) {
            var fileUpload = transferManager.uploadFile(uploadFileRequestBuilder ->
                    uploadFileRequestBuilder
                            .putObjectRequest(
                                    putObjectRequestBuilder -> putObjectRequestBuilder.bucket(bucketName).key(cosPath))
                            .source(originPath));
            var uploadResult = fileUpload.completionFuture().join();
            return uploadResult.response().eTag();
        }
    }

    public static String fileDownload(S3ClientConfiguration clientConfiguration, String bucketName, String storePath, String cosPath) {
        try (var transferManager = initTransferManager(clientConfiguration)) {
            var downloadFile = transferManager.downloadFile(
                    downloadFileRequestBuilder -> downloadFileRequestBuilder
                            .getObjectRequest(getObjectRequestBuilder ->
                                    getObjectRequestBuilder.bucket(bucketName).key(cosPath))
                            .destination(Paths.get(storePath)));
            var downloadResult = downloadFile.completionFuture().join();
            return downloadResult.response().eTag();
        }
    }

    private static S3TransferManager initTransferManager(S3ClientConfiguration clientConfiguration) {
        return S3TransferManager.builder()
                .s3ClientConfiguration(clientConfiguration)
                .build();
    }

}
