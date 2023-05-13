#### Executable jar

| key                | 目的                                                                                                                                                           |
|--------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| loader.path        | lib包加载路径                                                                                                                                                     |
| loader.home        | 用于解析loader.path中的相对路径。 例如，给定loader.path = lib，则$ {loader.home} / lib是类路径位置（以及该目录中的所有jar文件）。 此属性还用于查找loader.properties文件，如以下示例/ opt / app所示。它默认为$ {user.dir}。 |
| loader.args        | main方法的默认参数（以空格分隔）                                                                                                                                           |
| loader.main        | 要启动的主类的名称（例如com.app.Application）                                                                                                                             |
| loader.config.name | 属性文件的路径（例如，classpath：loader.properties）。 默认为loader.properties。                                                                                               |
| loader.system      | 布尔值标志，指示应将所有属性添加到系统属性。 默认为false。                                                                                                                             |

参考：[executable-jar.launching](https://docs.spring.io/spring-boot/docs/current/reference/html/executable-jar.html#executable-jar.launching)

