/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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
package com.unicorn.config

import com.unicorn.handler.SimRestErrorHandler
import org.apache.hc.client5.http.impl.classic.HttpClients
import org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManagerBuilder
import org.apache.hc.client5.http.ssl.NoopHostnameVerifier
import org.apache.hc.client5.http.ssl.SSLConnectionSocketFactory
import org.apache.hc.core5.ssl.SSLContexts
import org.springframework.beans.factory.annotation.Value
import org.springframework.boot.ssl.SslBundles
import org.springframework.boot.web.client.RestTemplateBuilder
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.http.client.ClientHttpRequestFactory
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory
import org.springframework.web.client.RestTemplate
import java.security.KeyManagementException
import java.security.KeyStoreException
import java.security.NoSuchAlgorithmException

@Configuration
class RestTemplateConfig {
    //boot -->spring   applicationContext.xml --- @Configuration配置   ConfigBean = applicationContext.xml
    //RestTemplate提供了多种便捷访问远程Http服务的方法，
    //是一种简单便捷的访问restful服务模板类，是Spring提供的用于访问Rest服务的客户端模板工具集
    // Should always use RestTemplateBuilder to creat restTemplate instance
    @Bean // @LoadBalanced//Spring Cloud Ribbon是基于Netflix Ribbon实现的一套客户端       负载均衡的工具。
    fun restTemplate(builder: RestTemplateBuilder, clientHttpRequestFactory: ClientHttpRequestFactory?): RestTemplate {
        val restTemplate = builder
            .errorHandler(SimRestErrorHandler()) //设置自定义异常处理
            .build()
        if (clientHttpRequestFactory != null) {
            restTemplate.requestFactory = clientHttpRequestFactory
        }
        return restTemplate
    }

    @Bean
    fun restSSLTemplate(
        builder: RestTemplateBuilder,
        sslBundles: SslBundles,
        @Value("\${web.ssl.bundle-name}") bundleName: String
    ): RestTemplate =
        builder
            .errorHandler(SimRestErrorHandler())
            .setSslBundle(sslBundles.getBundle(bundleName))
            .build()

    // If you have Spring WebFlux on your classpath, you can also choose to use WebClient to call remote REST services
//    private val webClient: WebClient? = null
//    this.webClient = webClientBuilder.baseUrl("https://example.org").apply(ssl.fromBundle("mybundle")).build();

    @Bean
    @Throws(NoSuchAlgorithmException::class, KeyStoreException::class, KeyManagementException::class)
    fun clientHttpRequestFactory(): HttpComponentsClientHttpRequestFactory {
        // Trust standard CA and those trusted by our custom strategy
        val sslContext = SSLContexts.custom() // Disabling SSL Certificate Validation in Spring RestTemplate
            // use a custom TrustStrategy that trusts all certs, and also use NoopHostnameVerifier() to disable hostname verification
            .loadTrustMaterial(null) { _, _ -> true }
            .build()
        val sslSocketFactory = SSLConnectionSocketFactory(sslContext, NoopHostnameVerifier.INSTANCE)
        val connMgr = PoolingHttpClientConnectionManagerBuilder.create()
            .setSSLSocketFactory(sslSocketFactory)
            .build()
        val httpClient = HttpClients.custom()
            .setConnectionManager(connMgr)
            .build()
        val requestFactory = HttpComponentsClientHttpRequestFactory()
        requestFactory.httpClient = httpClient
        return requestFactory
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
