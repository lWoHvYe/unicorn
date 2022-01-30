/*
 *    Copyright (c) 2021-2022.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.tools.service;

import org.springframework.web.multipart.MultipartFile;

/**
 * @date 2021年09月05日 16:20
 */
public interface IAliyunOSSService {

    /**
     * 分片上传
     *
     * @param file
     * @date 2021/9/5 17:12
     */
    void multipartUploadFile(MultipartFile file);

    /**
     * 断点续传下载
     *
     * @param ossUri       oss地址（无前缀）
     * @param downloadPath (下载路径)
     * @date 2021/9/5 17:12
     */
    void downloadFile(String ossUri, String downloadPath);

}
