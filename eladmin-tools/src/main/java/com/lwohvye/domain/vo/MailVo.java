package com.lwohvye.domain.vo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.springframework.web.multipart.MultipartFile;

import java.util.Date;

@Getter
@Setter
@ToString
@Accessors(chain = true)
public class MailVo {
    private String id;
    // 发送方
    private String from;
    // 接收方
    private String to;
    // 主题
    private String subject;
    // 正文
    private String text;
    // 发送时间
    private Date sentDate;

    private String cc;
    private String bcc;
    // 发送状态
    private String status;
    // 错误信息
    private String error;
    // 附件
    @JsonIgnore
    private MultipartFile[] multipartFiles;

}
