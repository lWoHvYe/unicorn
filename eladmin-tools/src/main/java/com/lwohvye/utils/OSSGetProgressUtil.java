package com.lwohvye.utils;

import com.aliyun.oss.OSS;
import com.aliyun.oss.OSSClientBuilder;
import com.aliyun.oss.event.ProgressEvent;
import com.aliyun.oss.event.ProgressEventType;
import com.aliyun.oss.event.ProgressListener;
import com.aliyun.oss.model.GetObjectRequest;
import com.aliyun.oss.model.PutObjectRequest;
import lombok.extern.slf4j.Slf4j;

import java.io.*;

/**
 * @author Hongyan Wang
 * @description 上传/下载进度条，适用于一般的上传下载，不适用断点和分片
 * @date 2021/9/5 17:37
 */
@Slf4j
public class OSSGetProgressUtil {

    /**
     * The uploading progress listener. Its progressChanged API is called by the SDK when there's an update.
     */
    public static class PutObjectProgressListener implements ProgressListener {

        private long bytesWritten = 0;
        private long totalBytes = -1;
        private boolean succeed = false;

        @Override
        public void progressChanged(ProgressEvent progressEvent) {
            long bytes = progressEvent.getBytes();
            ProgressEventType eventType = progressEvent.getEventType();
            switch (eventType) {
                case TRANSFER_STARTED_EVENT -> log.info("Start to upload......");
                case REQUEST_CONTENT_LENGTH_EVENT -> {
                    this.totalBytes = bytes;
                    log.info(this.totalBytes + " bytes in total will be uploaded to OSS");
                }
                case REQUEST_BYTE_TRANSFER_EVENT -> {
                    this.bytesWritten += bytes;
                    if (this.totalBytes != -1) {
                        int percent = (int) (this.bytesWritten * 100.0 / this.totalBytes);
                        log.info(bytes + " bytes have been written at this time, upload progress: " +
                                 percent + "%(" + this.bytesWritten + "/" + this.totalBytes + ")");
                    } else {
                        log.info(bytes + " bytes have been written at this time, upload ratio: unknown" +
                                 "(" + this.bytesWritten + "/...)");
                    }
                }
                case TRANSFER_COMPLETED_EVENT -> {
                    this.succeed = true;
                    log.info("Succeed to upload, " + this.bytesWritten + " bytes have been transferred in total");
                }
                case TRANSFER_FAILED_EVENT -> log.info("Failed to upload, " + this.bytesWritten + " bytes have been transferred");
                default -> log.info("unsupported eventType: {}", eventType.name());
            }
        }

        public boolean isSucceed() {
            return succeed;
        }
    }

    /**
     * The downloading progress listener. Its progressChanged API is called by the SDK when there's an update.
     */
    public static class GetObjectProgressListener implements ProgressListener {

        private long bytesRead = 0;
        private long totalBytes = -1;
        private boolean succeed = false;

        @Override
        public void progressChanged(ProgressEvent progressEvent) {
            long bytes = progressEvent.getBytes();
            ProgressEventType eventType = progressEvent.getEventType();
            switch (eventType) {
                case TRANSFER_STARTED_EVENT -> log.info("Start to download......");
                case RESPONSE_CONTENT_LENGTH_EVENT -> {
                    this.totalBytes = bytes;
                    log.info(this.totalBytes + " bytes in total will be downloaded to a local file");
                }
                case RESPONSE_BYTE_TRANSFER_EVENT -> {
                    this.bytesRead += bytes;
                    if (this.totalBytes != -1) {
                        int percent = (int) (this.bytesRead * 100.0 / this.totalBytes);
                        log.info(bytes + " bytes have been read at this time, download progress: " +
                                 percent + "%(" + this.bytesRead + "/" + this.totalBytes + ")");
                    } else {
                        log.info(bytes + " bytes have been read at this time, download ratio: unknown" +
                                 "(" + this.bytesRead + "/...)");
                    }
                }
                case TRANSFER_COMPLETED_EVENT -> {
                    this.succeed = true;
                    log.info("Succeed to download, " + this.bytesRead + " bytes have been transferred in total");
                }
                case TRANSFER_FAILED_EVENT -> log.info("Failed to download, " + this.bytesRead + " bytes have been transferred");
                default -> log.info("unsupported eventType: {}", eventType.name());
            }
        }

        public boolean isSucceed() {
            return succeed;
        }
    }

    public static void main(String[] args) {
        String endpoint = "http://oss-cn-hangzhou.aliyuncs.com";
        String accessKeyId = "<accessKeyId>";
        String accessKeySecret = "<accessKeySecret>";
        String bucketName = "<bucketName>";

        String key = "object-get-progress-sample";

        OSS client = new OSSClientBuilder().build(endpoint, accessKeyId, accessKeySecret);

        try {
            File fh = createSampleFile();

            // 带进度条的上传
            client.putObject(new PutObjectRequest(bucketName, key, fh).
                    withProgressListener(new PutObjectProgressListener()));

            // 带进度条的下载
            client.getObject(new GetObjectRequest(bucketName, key).
                    withProgressListener(new GetObjectProgressListener()), fh);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    /**
     * Create a temp file with about 50MB.
     */
    private static File createSampleFile() throws IOException {
        File file = File.createTempFile("oss-java-sdk-", ".txt");
        file.deleteOnExit();

        try (Writer writer = new OutputStreamWriter(new FileOutputStream(file))) {
            for (int i = 0; i < 1000; i++) {
                writer.write("abcdefghijklmnopqrstuvwxyz\n");
                writer.write("0123456789011234567890\n");
            }
        }

        return file;
    }

}
