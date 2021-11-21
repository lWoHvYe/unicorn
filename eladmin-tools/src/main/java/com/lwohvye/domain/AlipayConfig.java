/*
 *  Copyright 2019-2020 Zheng Jie
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.lwohvye.domain;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotBlank;
import java.io.Serializable;

/**
 * 支付宝配置类
 * @author Zheng Jie
 * @date 2018-12-31
 */
@Data
@Entity
@Table(name = "tool_alipay_config")
public class AlipayConfig implements Serializable {

    @Id
    @Column(name = "config_id")
    @Schema(description = "ID" , accessMode = Schema.AccessMode.READ_ONLY)
    private Long id;

    @NotBlank
    @Schema(description = "应用ID" )
    private String appId;

    @NotBlank
    @Schema(description = "商户私钥" )
    private String privateKey;

    @NotBlank
    @Schema(description = "支付宝公钥" )
    private String publicKey;

    @Schema(description = "签名方式" )
    private String signType="RSA2";

    @Column(name = "gateway_url")
    @Schema(description = "支付宝开放安全地址" , accessMode = Schema.AccessMode.READ_ONLY)
    private String gatewayUrl = "https://openapi.alipaydev.com/gateway.do";

    @Schema(description = "编码" , accessMode = Schema.AccessMode.READ_ONLY)
    private String charset= "utf-8";

    @NotBlank
    @Schema(description = "异步通知地址" )
    private String notifyUrl;

    @NotBlank
    @Schema(description = "订单完成后返回的页面" )
    private String returnUrl;

    @Schema(description = "类型" )
    private String format="JSON";

    @NotBlank
    @Schema(description = "商户号" )
    private String sysServiceProviderId;

}
