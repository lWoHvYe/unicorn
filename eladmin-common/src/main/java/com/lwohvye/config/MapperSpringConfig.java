/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.config;

import org.mapstruct.MapperConfig;

// MapStruct Spring Extensions 会自动生成一个适配类处理Mapper注册
@MapperConfig(componentModel = "spring")
// 默认情况下，生成的适配类将位于包org.mapstruct.extensions.spring.converter中，名称固定为ConversionServiceAdapter，可以通过下面的方式修改
// 如果Spring IoC容器中有多个ConversionService，可以通过@SpringMapperConfig注解的conversionServiceBeanName 参数指定。
// @SpringMapperConfig(conversionServiceAdapterPackage = "com.lwohvye.config",
//         conversionServiceAdapterClassName = "MapStructConversionServiceAdapter")
public class MapperSpringConfig {
}
