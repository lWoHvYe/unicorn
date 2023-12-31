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

package com.lwohvye.beans.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.annotation.ProxyTransactionManagementConfiguration;

@Configuration
public class CustomCommonConfig {


    /**
     * 基于anno的事务默认只对public的方法生效，通过下面的配置可以对protected和package-private也生效，在boot中也许默认配置了这个
     * <p>
     * Register a custom AnnotationTransactionAttributeSource with the
     * publicMethodsOnly flag set to false to enable support for
     * protected and package-private @Transactional methods in
     * class-based proxies.
     *
     * @see ProxyTransactionManagementConfiguration#transactionAttributeSource()
     */
/*    @Bean
    TransactionAttributeSource transactionAttributeSource() {
        return new AnnotationTransactionAttributeSource(false);
    }*/

}
