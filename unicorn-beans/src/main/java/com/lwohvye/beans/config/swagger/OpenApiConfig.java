/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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
package com.lwohvye.beans.config.swagger;

import com.lwohvye.core.utils.redis.RedisUtils;
import io.swagger.v3.oas.annotations.ExternalDocumentation;
import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.enums.SecuritySchemeIn;
import io.swagger.v3.oas.annotations.enums.SecuritySchemeType;
import io.swagger.v3.oas.annotations.info.Contact;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.info.License;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.security.SecurityScheme;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.redisson.api.RedissonClient;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/*
    @OpenAPIDefinition全局只能定义一个，主要配置文档信息和安全配置，这里列举了常用的请求头携带token的安全配置模式
    @OpenAPIDefinition下的info属性配置文档信息
    @OpenAPIDefinition下的security配置认证方式，name属性引入自定义的认证模式
    @SecurityScheme注解就是自定义的认证模式，配置请求头携带token
    注解配置到方式，后续可进行尝试
 */
@OpenAPIDefinition(
        info = @Info(
                title = "Unicorn Api Doc",
                version = "4.2.0",
                description = "1101100_1010111_1101111_1001000_1110110_1011001_1100101",
                contact = @Contact(name = "王红岩-_-lWoHvYe", url = "https://www.lwohvye.com", email = "lWoHvYe@outlook.com"),
                license = @License(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
        ),
        security = @SecurityRequirement(name = "Authorization"),
        externalDocs = @ExternalDocumentation(description = "SpringDoc Full Documentation", url = "https://springdoc.org")
)
@SecurityScheme(type = SecuritySchemeType.HTTP, name = "Authorization", scheme = "Bearer", in = SecuritySchemeIn.HEADER)
@Slf4j
@Configuration
public class OpenApiConfig {

/*
    @Autowired
    private SwaggerProperties swaggerProperties;

    @Value("${jwt.header:Authorization}")
    private String tokenHeader;

    @Bean
    public OpenAPI springDocOpenAPI() {
        //配置认证、请求头参数
        // https://swagger.io/docs/specification/authentication/
        // https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.0.3.md#securitySchemeObject
        // 接口调试路径
//        var tryServer = new Server().url(swaggerProperties.getTryHost());
        var contact = new Contact().name("王红岩-_-lWoHvYe").url("https://www.lwohvye.com").email("lWoHvYe@outlook.com");
        var license = new License().name("Apache 2.0").url("https://www.apache.org/licenses/LICENSE-2.0.html");
        return new OpenAPI()
                .components(components())
                // 要配置这个，Authorization的Header才生效
                .security(securityRequirements())
//                .servers(Collections.singletonList(tryServer))
                .info(new Info()
                        .title(swaggerProperties.getApplicationName() + " API Doc") // 应用程序编程接口 Application Programming Interface
                        .contact(contact)
                        .description(swaggerProperties.getApplicationDescription())
                        .version("Application Version: " + swaggerProperties.getApplicationVersion() + "\n Spring Boot Version: " + SpringBootVersion.getVersion())
                        .license(license)
                )
                .externalDocs(new ExternalDocumentation()
                        .description("SpringDoc Full Documentation")
                        .url("https://springdoc.org/")
                );
    }

    private Components components() {
        // 在这里定义的SecurityScheme，对应需放到header中的，需要在SecurityRequirement中配置使用的地方，在接口文档处，请求才会带上
        return new Components()
                .addSecuritySchemes(tokenHeader, new SecurityScheme().type(SecurityScheme.Type.APIKEY).in(SecurityScheme.In.HEADER).name(tokenHeader).description("token令牌"))
                .addSecuritySchemes("bearerAuth", new SecurityScheme().type(SecurityScheme.Type.HTTP).scheme("bearer").bearerFormat("JWT"))
                .addSecuritySchemes("basicScheme", new SecurityScheme().type(SecurityScheme.Type.HTTP).scheme("basic"))
                // 这个在下面配置了全局请求头，所以都会带上
                .addParameters("SpInfo", new HeaderParameter().name("platform").description("平台信息（标识）").schema(new StringSchema()).required(false));
    }

    private List<SecurityRequirement> securityRequirements() {
        var securityRequirements = new ArrayList<SecurityRequirement>();
        // 这里当前是所有请求都带上。理论上是有比较优雅都方式，配置只有符合特定规则的才带上
        var authHeader = new SecurityRequirement().addList(tokenHeader);
        securityRequirements.add(authHeader);
        return securityRequirements;
    }

    // Ant URI相关参考：https://www.lwohvye.com/2021/11/27/spring-mvc-%e8%b7%af%e5%be%84uri%e4%b8%ad%e7%9a%84-ant-%e9%a3%8e%e6%a0%bc/
    // ^(?!auth).*$ 表示所有包含auth的接口不需要使用securitySchemes即不需要带token
    // ^标识开始  ()里是一子表达式  ?!/auth表示匹配不是/auth的位置，匹配上则添加请求头，注意路径已/开头  .表示任意字符  *表示前面的字符匹配多次 $标识结束


    */
/**
 * 添加全局的请求头参数
 */
/*

    @Bean
    public OpenApiCustomizer customerGlobalHeaderOpenApiCustomizer() {
        return openApi -> openApi.getPaths().values().stream().flatMap(pathItem -> pathItem.readOperations().stream())
                .forEach(operation -> operation.addParametersItem(new HeaderParameter().$ref("#/components/parameters/SpInfo")));
    }
*/

    /**
     * bean初始化前后的两个切入点
     *
     * @return org.springframework.beans.factory.config.BeanPostProcessor
     * @date 2022/3/18 7:23 PM
     */
    @Bean
    public static BeanPostProcessor whyBeanPostProcessor() { // 有些忘了，这里整成static是否有别的考虑
        return new BeanPostProcessor() {

            @Override
            public Object postProcessBeforeInitialization(@NotNull Object bean, @NotNull String beanName) throws BeansException {
                if (bean instanceof RedisUtils ru)
                    log.info("The module of the RedisUtils is {}", ru.getClass().getModule());
                return BeanPostProcessor.super.postProcessBeforeInitialization(bean, beanName);
            }

            @Override
            public Object postProcessAfterInitialization(@NotNull Object bean, @NotNull String beanName) throws BeansException {
                if (bean instanceof RedissonClient rc)
                    log.info("The id of the RedissonClient is {}", rc.getId());
                return BeanPostProcessor.super.postProcessAfterInitialization(bean, beanName);
            }
        };
    }
}
