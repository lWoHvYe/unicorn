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
package com.lwohvye.tools.service

import org.springframework.web.multipart.MultipartFile

/**
 * @date 2021年09月05日 16:20
 */
interface ICOSService {
    /**
     * 分片上传
     *
     * @param file
     * @param cosPath
     * @date 2021/9/5 17:12
     */
    fun upload(file: MultipartFile, cosPath: String?)

    /**
     * 断点续传下载
     *
     * @param storePath (存储路径)
     * @param cosPath   cos地址（无前缀）
     * @date 2021/9/5 17:12
     */
    fun download(storePath: String?, cosPath: String?)
}
