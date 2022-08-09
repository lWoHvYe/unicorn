在Spring Boot 2.6.0版本，启动报错PatternsRequestCondition.getPatterns()空指针，原因详见springfox的[issues](https://github.com/springfox/springfox/issues/3462)
，扩展 [URL Matching with PathPattern in Spring MVC](https://spring.io/blog/2020/06/30/url-matching-with-pathpattern-in-spring-mvc) 。该版本Spring
boot的 [ Release-Notes ](https://github.com/spring-projects/spring-boot/wiki/Spring-Boot-2.6-Release-Notes)

导致报错的原因是：

- 在SpringMVC的5.3.x系列版本（Spring Boot 2.6.x），引入新的URL Matching方式PathPattern。之前已有的是AntPathMatcher。
- 在Spring Boot 2.6.0版本，将默认的调整为PathPattern。并提供配置 `spring.mvc.pathmatch.matching-strategy=ant_path_matcher`
  可以切换回AntPathMatcher，但是`The actuator endpoints now also use PathPattern based URL matching. Note that the path matching strategy for actuator endpoints is not configurable via a configuration property.`
- 导致报错的就是webEndpointServletHandlerMapping方法的`/actuator/health/**、/actuator/health、/actuator`这几个方法。所以在找到让springfox忽略（不处理）这几个方法的方案前。~~还未找到好的解决方案~~，过滤方式详见方法三
- ~~暂通过改源码解决，期待后续方案。📦后的jar详见ex-lib目录~~。[git commit](https://github.com/lWoHvYe/springfox/commit/9cb5e727a48e815b73461793ad37eae73c4af0e7)
- 调整源码修改方式，ex-lib中jar为该方式。详见：[git commit](https://github.com/lWoHvYe/springfox/commit/1dfca11330435e1c8965c93d1fd3943016c63062)
- 生活总是充满惊喜。上面说了，导致问题的原因是/actuator/**，这些是actuator模块的，项目并未显式的引用，所以为神马会有这几个path？ 🤪答案就是redisson。排除掉就可以了，至少只要不需要这些功能，不用改源码
- 第三种修复方式更为推荐。具体为将springfox中springfox.documentation.spring.web.plugins包下的WebMvcRequestHandlerProvider.java拷贝到项目下(包路径不要变)，进行修改，主体为过滤掉PatternsRequestCondition为null的handlerMappings。详见：[git commit](https://github.com/lWoHvYe/eladmin/commit/e4c94d2c6e18d474a6b2b620cd78e4e5464419b4) , [扩展](https://www.lwohvye.com/2021/11/30/%e6%b5%85%e8%b0%88%e5%9c%a8jar%e4%b8%ad%e5%90%8c%e5%90%8d%e7%b1%bb%e5%86%b2%e7%aa%81%e9%97%ae%e9%a2%98%e5%8f%8a%e8%a6%86%e5%86%99%e7%ac%ac%e4%b8%89%e6%96%b9jar%e4%b8%ad%e7%9a%84%e7%b1%bb/)
- 第四种修复方式更好一些。通过实现BeanPostProcessor，在bean初始化前后插入一些操作。详见：[git commit](https://github.com/lWoHvYe/eladmin/commit/5261b859ac5ff7e96e38894c5005355991d6d0ba) , [出处](https://github.com/springfox/springfox/issues/3462#issuecomment-983144080)
- 一直有新的修复方式加入，针对问题的不同切入点
- 之前一直没看懂，但最先接触到的下面这种定义 WebMvcEndpointHandlerMapping Bean的方式就是一种解决方案，具体细节还没搞懂，但跟上面这些是不同的方式，上面都是在处理前exclude the `/actuator` 系列，这种方式是为其设置了patternsCondition (在构造WebMvcEndpointHandlerMapping时，不传pathPatternParser或传null，就会有以上结果)

⌚️马上🕑了。天亮再继续。考虑从springfox迁移到springdoc了

https://github.com/spring-projects/spring-boot/issues/24645

https://github.com/spring-projects/spring-boot/issues/24805

https://github.com/spring-projects/spring-boot/issues/21694

https://github.com/spring-projects/spring-framework/issues/24952

https://stackoverflow.com/questions/69108273/spring-boot-swagger-documentation-doesnt-work/69814964

https://github.com/springfox/springfox/issues/3462

If one insists on continuing to use Springfox with Spring Boot >= 2.6, one can try to force use of Ant Path Matching by setting

```yaml
spring.mvc.pathmatch.matching-strategy=ant_path_matcher
```

Forcing Ant Path Matching on the actuators is a separate problem. It works by injecting the WebMvcEndpointHandlerMapping that was auto-configured before the
change by WebMvcEndpointManagementContextConfiguration:

```java
@Bean
public WebMvcEndpointHandlerMapping webEndpointServletHandlerMapping(
    WebEndpointsSupplier webEndpointsSupplier,
    ServletEndpointsSupplier servletEndpointsSupplier, ControllerEndpointsSupplier controllerEndpointsSupplier,
    EndpointMediaTypes endpointMediaTypes, CorsEndpointProperties corsProperties,
    WebEndpointProperties webEndpointProperties, Environment environment) {
  List<ExposableEndpoint<?>> allEndpoints = new ArrayList<>();
  Collection<ExposableWebEndpoint> webEndpoints = webEndpointsSupplier.getEndpoints();
  allEndpoints.addAll(webEndpoints);
  allEndpoints.addAll(servletEndpointsSupplier.getEndpoints());
  allEndpoints.addAll(controllerEndpointsSupplier.getEndpoints());
  String basePath = webEndpointProperties.getBasePath();
  EndpointMapping endpointMapping = new EndpointMapping(basePath);
  boolean shouldRegisterLinksMapping = shouldRegisterLinksMapping(webEndpointProperties, environment, basePath);
  return new WebMvcEndpointHandlerMapping(endpointMapping, webEndpoints, endpointMediaTypes,
      corsProperties.toCorsConfiguration(), new EndpointLinksResolver(allEndpoints, basePath),
      shouldRegisterLinksMapping);
}

private boolean shouldRegisterLinksMapping(WebEndpointProperties webEndpointProperties, Environment environment,
    String basePath) {
  return webEndpointProperties.getDiscovery().isEnabled() && (StringUtils.hasText(basePath)
      || ManagementPortType.get(environment).equals(ManagementPortType.DIFFERENT));
}
```

There may be a cleverer way by excluding the actuators from being analyzed by Springfox in the first place.

You're mileage may vary. Switching to springdoc is probably the more worthwhile approach.

