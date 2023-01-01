/*
 *    Copyright (c) 2022-2023.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.starter.config;

import com.lwohvye.starter.modules.handler.SimRestErrorHandler;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.ssl.SSLContexts;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

import javax.net.ssl.SSLContext;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;

@Configuration
public class RestConfig {

    //boot -->spring   applicationContext.xml --- @Configuration配置   ConfigBean = applicationContext.xml
    //RestTemplate提供了多种便捷访问远程Http服务的方法，
    //是一种简单便捷的访问restful服务模板类，是Spring提供的用于访问Rest服务的客户端模板工具集
    // Should always use RestTemplateBuilder to creat restTemplate instance
    @Bean
    // @LoadBalanced//Spring Cloud Ribbon是基于Netflix Ribbon实现的一套客户端       负载均衡的工具。
    public RestTemplate restTemplate(RestTemplateBuilder builder) {
        // Trust standard CA and those trusted by our custom strategy
        SSLContext sslContext;
        try {
            // Disabling SSL Certificate Validation in Spring RestTemplate
            // use a custom TrustStrategy that trusts all certs, and also use NoopHostnameVerifier() to disable hostname verification
            sslContext = SSLContexts.custom().loadTrustMaterial(null, (x509Certificates, authType) -> true).build();
        } catch (NoSuchAlgorithmException | KeyStoreException | KeyManagementException e) {
            throw new RuntimeException(e);
        }
        var sslSocketFactory = new SSLConnectionSocketFactory(sslContext, NoopHostnameVerifier.INSTANCE);
        var httpClient = HttpClients.custom().setSSLSocketFactory(sslSocketFactory).build();
        var requestFactory = new HttpComponentsClientHttpRequestFactory();
        requestFactory.setHttpClient(httpClient);

        var restTemplate = builder.errorHandler(new SimRestErrorHandler()).build(); //设置自定义异常处理
        restTemplate.setRequestFactory(requestFactory);

        return restTemplate;
    }

    /*
    @Bean//配置负载均衡算法
    public IRule myRule() {
        //return new RoundRobinRule();
        //return new RandomRule();//达到的目的，用我们重新选择的随机算法替代默认的轮询。
        return new RetryRule();
    }
    */
}
