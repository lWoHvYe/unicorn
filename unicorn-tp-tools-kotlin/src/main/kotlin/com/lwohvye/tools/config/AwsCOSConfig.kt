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
package com.lwohvye.tools.config

import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import software.amazon.awssdk.auth.credentials.AwsCredentials
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.transfer.s3.S3ClientConfiguration
import software.amazon.awssdk.transfer.s3.SizeConstant

@Configuration
class AwsCOSConfig {
    @Bean
    fun s3ClientConfiguration(cosProperties: AwsCOSProperties): S3ClientConfiguration {
        return S3ClientConfiguration.builder()
            .credentialsProvider {
                object : AwsCredentials {
                    override fun accessKeyId(): String {
                        return cosProperties.accessKeyId!!
                    }

                    override fun secretAccessKey(): String {
                        return cosProperties.secretAccessKey!!
                    }
                }
            }
            .region(Region.of(cosProperties.region))
            .minimumPartSizeInBytes(10 * SizeConstant.MB)
            .targetThroughputInGbps(20.0)
            .build()
    }
}
