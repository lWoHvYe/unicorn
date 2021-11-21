package com.lwohvye.config;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.HeaderParameter;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import lombok.RequiredArgsConstructor;
import org.springdoc.core.customizers.OpenApiCustomiser;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringBootVersion;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/*
    @OpenAPIDefinition全局只能定义一个，主要配置文档信息和安全配置，这里列举了常用的请求头携带token的安全配置模式
    @OpenAPIDefinition下的info属性配置文档信息
    @OpenAPIDefinition下的security配置认证方式，name属性引入自定义的认证模式
    @SecurityScheme注解就是自定义的认证模式，配置请求头携带token
    注解配置到方式，后续可进行尝试
 */
// @OpenAPIDefinition(
//         info = @io.swagger.v3.oas.annotations.info.Info(
//                 title = "EL-Admin Api Doc",
//                 version = "3.0.0",
//                 description = "'1101100_1010111_1101111_1001000_1110110_1011001_1100101' + SpringBootVersion.getVersion()",
//                 contact = @Contact(name = "lWoHvYe")
//         ),
//         security = @SecurityRequirement(name = "Authorization"),
//         externalDocs = @io.swagger.v3.oas.annotations.ExternalDocumentation(description = "参考文档",
//                 url = "https://github.com/swagger-api/swagger-core/wiki/Swagger-2.X---Annotations"
//         )
// )
// @io.swagger.v3.oas.annotations.security.SecurityScheme(type = SecuritySchemeType.HTTP, name = "Authorization", scheme = "Bearer", in = SecuritySchemeIn.HEADER)
@Configuration
@RequiredArgsConstructor
public class OpenApiConfig {

    private final SwaggerProperties swaggerProperties;

    @Value("${jwt.header:Authorization}")
    private String tokenHeader;

    @Value("${jwt.token-start-with:Bearer }")
    private String tokenStartWith;

    @Bean
    public OpenAPI springDocOpenAPI() {
        //配置认证、请求头参数
        // https://swagger.io/docs/specification/authentication/
        // https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.0.3.md#securitySchemeObject
        // 接口调试路径
        var tryServer = new Server().url(swaggerProperties.getTryHost());
        var contact = new Contact().name("王红岩-_-lWoHvYe").url("https://www.lwohvye.com").email("lWoHvYe@outlook.com");
        var license = new License().name("Apache 2.0").url("https://www.apache.org/licenses/LICENSE-2.0.html");
        return new OpenAPI()
                .components(components())
                // 要配置这个，Authorization的Header才生效
                .security(securityRequirements())
                .servers(Collections.singletonList(tryServer))
                .info(new Info()
                        .title(swaggerProperties.getApplicationName() + " Api Doc")
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

    // ^(?!auth).*$ 表示所有包含auth的接口不需要使用securitySchemes即不需要带token
    // ^标识开始  ()里是一子表达式  ?!/auth表示匹配不是/auth的位置，匹配上则添加请求头，注意路径已/开头  .表示任意字符  *表示前面的字符匹配多次 $标识结束


    /**
     * 添加全局的请求头参数
     */
    @Bean
    public OpenApiCustomiser customerGlobalHeaderOpenApiCustomiser() {
        return openApi -> openApi.getPaths().values().stream().flatMap(pathItem -> pathItem.readOperations().stream())
                .forEach(operation -> operation.addParametersItem(new HeaderParameter().$ref("#/components/parameters/SpInfo")));
    }

}
