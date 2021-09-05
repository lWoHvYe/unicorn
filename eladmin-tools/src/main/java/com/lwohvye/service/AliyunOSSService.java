package com.lwohvye.service;

import org.springframework.web.multipart.MultipartFile;

/**
 * @author Hongyan Wang
 * @date 2021年09月05日 16:20
 */
public interface AliyunOSSService {

    /**
     * @description 分片上传
     * @author Hongyan Wang
     * @date 2021/9/5 17:12
     * @param file
     */
    void multipartUploadFile(MultipartFile file);

    /**
     * @description 断点续传下载
     * @author Hongyan Wang
     * @date 2021/9/5 17:12
     * @param ossUri oss地址（无前缀）
     * @param downloadPath (下载路径)
     */
    void downloadFile(String ossUri,String downloadPath);

}
