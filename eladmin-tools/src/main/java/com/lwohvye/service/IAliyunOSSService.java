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
package com.lwohvye.service;

import org.springframework.web.multipart.MultipartFile;

/**
 * @date 2021年09月05日 16:20
 */
public interface IAliyunOSSService {

    /**
     * @param file
     * @description 分片上传
     * @date 2021/9/5 17:12
     */
    void multipartUploadFile(MultipartFile file);

    /**
     * @param ossUri       oss地址（无前缀）
     * @param downloadPath (下载路径)
     * @description 断点续传下载
     * @date 2021/9/5 17:12
     */
    void downloadFile(String ossUri, String downloadPath);

}
