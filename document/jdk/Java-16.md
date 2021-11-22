---
**Java16**之后，默认强封装JDK内部类，详见[JEP 396](https://openjdk.java.net/jeps/396) [JEP 403](https://openjdk.java.net/jeps/403) ，需在启动时添加相关参数。较简单的是添加
``--add-opens java.base/java.lang=ALL-UNNAMED`` ，也可根据需要缩小范围

后台运行jar（开启远程调试端口5005）

```shell
nohup java --add-opens java.base/java.lang=ALL-UNNAMED -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005 -jar eladmin-starter-2.6.17.jar >nohup.out 2>&1 &
```

---

#### Run

```
    针对异常
    java.lang.reflect.InaccessibleObjectException: Unable to make field private final java.lang.String java.lang.NamedPackage.name accessible: module java.base does not "opens java.lang" to unnamed module @7bfcd12c
    需要在jvm 中添加启动参数
     --add-opens java.base/java.lang=ALL-UNNAMED
    如果反射代码在命名模块中，则ALL-UNNAMED可以用其名称替换。
    如果要添加的标志太多，则可以考虑使用封装终止开关 --permit-illegal-access。它将允许类路径上的所有代码反射所有已命名的模块
    针对Java 16下，无法反射java.base包下的内容的情况，在mvn的运行环境中添加如下配置
        --illegal-access
        该参数有四个可选值：
        permit：Java 9开始默认值，允许通过反射访问，因此会提示像上面一样的警告，这个是首次非法访问警告，后续不警告
        warn：每次非法访问都会警告
        debug：在warn的基础上加入了类似e.printStackTrace()的功能
        deny：Java 16开始默认值。禁止所有的非法访问除了使用特别的命令行参数排除的模块，比如使用--add-opens排除某些模块使其能够通过非法反射访问
        Java 17移除了 --illegal-access，需要在mvn中使用--add-opens配置
        
```

#### Maven

```xml
            <!-- 解决 Java-16下 lombok报错：
            Fatal error compiling: java.lang.IllegalAccessError: class lombok.javac.apt.LombokProcessor (in unnamed module @0x486bcaa) cannot access class com.sun.tools.javac.processing.JavacProcessingEnvironment (in module jdk.compiler) because module jdk.compiler does not export com.sun.tools.javac.processing to unnamed module @0x486bcaa
            在最新的JDK-16构建中看到的异常是由于jep396：默认情况下强封装JDK内部构件。Lombok正在使用反射访问一个内部jdkapi，在以前的Java版本中，这会导致警告消息，现在则会导致硬错误。
            一般来说，在运行java时，通过在运行java时传递--add-opens=<module>/<package>=<accessing module>指令作为VM参数，可以显式地打开内部JDK包进行反射。在这种情况下，需要将这些指令传递给调用javac时运行的java进程。这可以通过在传递给javac的选项前面加上-J来完成，后者将把它传递给底层JVM。
            其中需要的选项在配置中使用<compilerArgs>元素传递。
            注意，我在选项前面添加了-J，以便将它们传递给运行javac的JVM，而不是javac选项。
            在问题中列出的--add-opens指令之外，还有一个额外的：
                -J--add-opens=jdk.compiler/com.sun.tools.javac.jvm=ALL-UNNAMED
            也是需要的。
            <fork>true</fork>也是必需的，否则-J选项将被忽略（从mvn clean install -X的输出判断）。查看Maven文档，在使用<compilerArgs>时，似乎需要随时将fork设置为true：
            参考：https://maven.apache.org/plugins/maven-compiler-plugin/compile-mojo.html#compilerArgs
            <compilerArgs>设置在fork设置为true时传递给编译器的参数。
            -->
<plugin>
    <!-- maven-compiler-plugin应该是默认使用的。所以可以不显示的配置。这里保留主要是为了记录 --add-opens=... 配置的方式 -->
    <groupId>org.apache.maven.plugins</groupId>
    <artifactId>maven-compiler-plugin</artifactId>
    <configuration>
        <source>17</source>
        <target>17</target>
        <!--<release>16</release>-->
        <!--<fork>true</fork>-->
        <!-- The flowing is for lombok version before 1.18.20 -->
        <compilerArgs>
            <arg>--enable-preview</arg>
            <arg>-Xlint:all</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.comp=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.file=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.main=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.model=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.parser=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.processing=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED</arg>
            <arg>-J--add-opens=jdk.compiler/com.sun.tools.javac.jvm=ALL-UNNAMED</arg>
        </compilerArgs>
        <!--for unmappable characters in classes-->
        <!-- <encoding>UTF-8</encoding>-->
        <showDeprecation>true</showDeprecation>
        <showWarnings>true</showWarnings>
        <!--for lombok annotations to resolve-->
        <!--contradictory to maven, intelliJ fails with this-->
        <annotationProcessorPaths>
            <!--4lombok-->
            <path>
                <groupId>org.projectlombok</groupId>
                <artifactId>lombok</artifactId>
                <!-- Support for Java 16
                https://github.com/rzwitserloot/lombok/issues/2681#
                -->
                <version>${lombok.version}</version>
            </path>
            <!--4mapstruct-->
            <path>
                <groupId>org.mapstruct</groupId>
                <artifactId>mapstruct-processor</artifactId>
                <version>${mapstruct.version}</version>
            </path>
        </annotationProcessorPaths>
        <!--https://docs.spring.io/spring-boot/docs/2.4.4/reference/html/appendix-configuration-metadata.html#configuration-metadata-annotation-processor-->
        <!--<proc>none</proc>-->
    </configuration>
</plugin>
```

#### lombok

在Java 16+的Runtime。需要lombok使用1.18.20+的版本

[Support for JDK16](https://github.com/rzwitserloot/lombok/issues/2681#)

