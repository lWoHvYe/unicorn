// This file is auto-generated, don't edit it. Thanks.
package com.lwohvye.utils;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import com.aliyun.dysmsapi20170525.Client;
import com.aliyun.dysmsapi20170525.models.QuerySendDetailsRequest;
import com.aliyun.dysmsapi20170525.models.QuerySendDetailsResponse;
import com.aliyun.dysmsapi20170525.models.SendSmsRequest;
import com.aliyun.dysmsapi20170525.models.SendSmsResponse;
import com.aliyun.teaopenapi.models.Config;
import com.lwohvye.config.AliCloudConfig;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import javax.validation.constraints.NotNull;

@Slf4j
public class DySMSUtil {

    @SneakyThrows
    public static Client createClient() {
        Config config = new Config();
        config.accessKeyId = AliCloudConfig.ACCESS_KEY_ID;
        config.accessKeySecret = AliCloudConfig.ACCESS_KEY_SECRET;
        return new Client(config);
    }

    /**
     * @param phoneNumbers
     * @param signName
     * @param templateCode
     * @param templateParam
     * @return java.lang.String
     * @description 发送短信
     * @date 2021/11/7 12:53 下午
     */
    @SneakyThrows
    public static String sendSms(String phoneNumbers, String signName, String templateCode, String templateParam) {
        Client client = DySMSUtil.createClient();
        // 1.发送短信
        SendSmsRequest sendReq = new SendSmsRequest()
                .setPhoneNumbers(phoneNumbers)
                .setSignName(signName)
                .setTemplateCode(templateCode)
                // JSON格式，对应模版中的占位
                .setTemplateParam(templateParam);
        SendSmsResponse sendResp = client.sendSms(sendReq);
        String code = sendResp.body.code;
        if (!StrUtil.equals(code, "OK")) {
            log.error("错误信息: {} ", sendResp.body.message);
            return null;
        }
        return sendResp.body.bizId;
    }

    /**
     * @param phoneNumbers
     * @param bizId
     * @description 查询结果
     * @date 2021/11/7 12:52 下午
     */
    @SneakyThrows
    public static void querySendDetails(@NotNull String phoneNumbers, String bizId) {
        Client client = DySMSUtil.createClient();
        // 3.查询结果
        for (String phoneNum : phoneNumbers.split(",")) {
            QuerySendDetailsRequest queryReq = new QuerySendDetailsRequest()
                    .setPhoneNumber(phoneNum.trim())
                    .setBizId(bizId)
                    .setSendDate(DateUtil.today())
                    .setPageSize(10L)
                    .setCurrentPage(1L);
            QuerySendDetailsResponse queryResp = client.querySendDetails(queryReq);
            var dtos = queryResp.body.smsSendDetailDTOs.smsSendDetailDTO;
            // 打印结果
            for (var dto : dtos) {
                if (ObjectUtil.equals(dto.sendStatus, 3L)) {
                    log.info(" {} 发送成功，接收时间: {} ", dto.phoneNum, dto.receiveDate);
                } else if (ObjectUtil.equals(dto.sendStatus, 2L)) {
                    log.info(" {} 发送失败", dto.phoneNum);
                } else {
                    log.info(" {} 正在发送中...", dto.phoneNum);
                }
            }
        }
    }
}
