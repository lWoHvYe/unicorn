---
#### MapStruct介绍

| Option                                           | Purpose                                                      | Default   |
| :----------------------------------------------- | :----------------------------------------------------------- | :-------- |
| `mapstruct. suppressGeneratorTimestamp`          | If set to `true`, the creation of a time stamp in the `@Generated` annotation in the generated mapper classes is suppressed. | `false`   |
| `mapstruct.verbose`                              | If set to `true`, MapStruct in which MapStruct logs its major decisions. Note, at the moment of writing in Maven, also `showWarnings` needs to be added due to a problem in the maven-compiler-plugin configuration. | `false`   |
| `mapstruct. suppressGeneratorVersionInfoComment` | If set to `true`, the creation of the `comment` attribute in the `@Generated` annotation in the generated mapper classes is suppressed. The comment contains information about the version of MapStruct and about the compiler used for the annotation processing. | `false`   |
| `mapstruct.defaultComponentModel`                | The name of the component model (see [Retrieving a mapper](https://mapstruct.org/documentation/stable/reference/html/#retrieving-mapper)) based on which mappers should be generated.<br>Supported values are:<br>`default`: the mapper uses no component model, instances are typically retrieved via `Mappers#getMapper(Class)`<br>`cdi`: the generated mapper is an application-scoped CDI bean and can be retrieved via `@Inject`<br>`spring`: the generated mapper is a singleton-scoped Spring bean and can be retrieved via `@Autowired`<br>`jsr330`: the generated mapper is annotated with {@code @Named} and can be retrieved via `@Inject`, e.g. using Spring <br> If a component model is given for a specific mapper via `@Mapper#componentModel()`, the value from the annotation takes precedence. | `default` |
| `mapstruct.defaultInjectionStrategy`             | The type of the injection in mapper via parameter `uses`. This is only used on annotated based component models such as CDI, Spring and JSR 330.<br>Supported values are:<br>`field`: dependencies will be injected in fields<br>`constructor`: will be generated constructor. Dependencies will be injected via constructor.<br>When CDI `componentModel` a default constructor will also be generated. If a injection strategy is given for a specific mapper via `@Mapper#injectionStrategy()`, the value from the annotation takes precedence over the option. | `field`   |
| `mapstruct.unmappedTargetPolicy`                 | The default reporting policy to be applied in case an attribute of the target object of a mapping method is not populated with a source value.<br>Supported values are:<br>`ERROR`: any unmapped target property will cause the mapping code generation to fail<br>`WARN`: any unmapped target property will cause a warning at build time<br>`IGNORE`: unmapped target properties are ignored<br>If a policy is given for a specific mapper via `@Mapper#unmappedTargetPolicy()`, the value from the annotation takes precedence. | `WARN`    |

MapStruct 提供的重要注解 :

@Mapper : 标记这个接口作为一个映射接口，并且是编译时 MapStruct 处理器的入口

@Mapping : 解决源对象和目标对象中，属性名字不同的情况

---
#### 循环依赖
解决办法：[Jpa实体间关系及MapStruct循环依赖相关](https://www.lwohvye.com/2020/12/01/manytomany%e6%88%96onetomany-manytoone%e5%af%bc%e8%87%b4%e5%be%aa%e7%8e%af%e4%be%9d%e8%b5%96%e7%9a%84%e9%97%ae%e9%a2%98-java-lang-stackoverflowerror-jpa/)
