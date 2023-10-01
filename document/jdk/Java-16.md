**Java16**之后，默认强封装JDK内部类，详见[JEP 396](https://openjdk.java.net/jeps/396) [JEP 403](https://openjdk.java.net/jeps/403) ，需在启动时添加相关参数。较简单的是添加
``--add-opens java.base/java.lang=ALL-UNNAMED`` ，也可根据需要缩小范围

后台运行jar（开启远程调试端口5005）

```shell
nohup java --add-opens java.base/java.lang=ALL-UNNAMED -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005 -jar unicorn-starter-2.6.18.jar >nohup.out 2>&1 &
```

#### Run

```
    针对异常
    java.lang.reflect.InaccessibleObjectException: Unable to make field private final java.lang.String java.lang.NamedPackage.name accessible: module java.base does not "opens java.lang" to unnamed module @7bfcd12c
    需要在jvm 中添加启动参数
     --add-opens java.base/java.lang=ALL-UNNAMED 
     注：在Java 9 中引入了Java平台模块系统（JPMS），这里的ALL-UNNAMED表示所有未命名模块。整体意思为将java.base/java.lang对所有未命名模块开放
     根据java.base模块的module-info.java文件，其已将主要的所有的类exports。在运行时需要反射获取java.base/java.lang下的类，所以要添加opens，这里应该是未把其子包open的
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

#### Java平台模块系统（JPMS）、Jigsaw 项目

这个确切的讲，是在Java 9引入的

```
JPMS对包可见性细化为：public to everyone、public but only to friend modules、public only within a module、protected、package、private
模块系统的首要目的是为了封装。然后在有些时候，我们必须要打破封装来处理遗留代码或是运行测试。我们可以下面几个命令行参数来打破封装。
需注意的是，包是没有层级关系的，也就是说java.base/java.lang配置只针对该包下的类，是不包括子包的，也就是针对java.lang.reflect要单独配置
--add-reads module=target-module(,target-module)*：更新源模块来读取目标模块。目标模块可以是ALL-UNNAMED来读取所有未命名模块。
--add-exports module/package=target-module(,target-module)*：更新源模块来导出包到目标模块。这会添加一个从源模块来目标模块的受限导出。目标模块可以是ALL-UNNAMED来导出到所有未命名模块。
--add-opens module/package=target-module(,target-module)*：更新源模块来开放包到目标模块。这回添加一个从源模块到目标模块的受限开放。
--patch-module module=file(;file)*：使用JAR文件或目录中的类和资源文件来覆盖或增加一个模块的内容。在需要临时修改一个模块的内容以方便测试时，--patch-module非常实用。
```

Java 为 module-info.java 设计了专用的语法

```
语法解读：(这里同样是不包括子包)

[open] module <module>: 声明一个模块，模块名称应全局唯一，不可重复。加上 open 关键词表示模块内的所有包都允许通过 Java 反射访问，模块声明体内不再允许使用 opens 语句。

requires [transitive | static] <module>: 声明模块依赖，一次只能声明一个依赖，如果依赖多个模块，需要多次声明。加上 transitive 关键词表示传递依赖，比如模块 A 依赖模块 B，模块 B 传递依赖模块 C，那么模块 A 就会自动依赖模块 C，类似于 Maven。加上 static 关键词表示编译时必需，运行时可选，类似于 Maven 的 <scope>runtime</scope>。

exports <package> [to <module1>[, <module2>...]]: 导出模块内的包（允许直接 import 使用），一次导出一个包，如果需要导出多个包，需要多次声明。如果需要定向导出，可以使用 to关键词，后面加上模块列表（逗号分隔）。

opens <package> [to <module>[, <module2>...]]: 开放模块内的包（允许通过 Java 反射访问），一次开放一个包，如果需要开放多个包，需要多次声明。如果需要定向开放，可以使用 to关键词，后面加上模块列表（逗号分隔）。

provides <interface | abstract class> with <class1>[, <class2> ...]: 声明模块提供的 Java SPI 服务，一次可以声明多个服务实现类（逗号分隔）。

uses <interface | abstract class>: 声明模块依赖的 Java SPI 服务，加上之后模块内的代码就可以通过 ServiceLoader.load(Class) 一次性加载所声明的 SPI 服务的所有实现类。
```

在实际使用中，

- https://www.oracle.com/corporate/features/understanding-java-9-modules.html
- An exports module directive specifies one of the module’s packages whose public types (and their nested public and protected types) should be accessible to
  code in all other modules.
- opens is used to specify `Allowing runtime-only access to a package`
- open module if used to specify `Allowing runtime-only access to all packages in a module`
- 如果一个package是exports的，我们可以在其他modules中通过反射创建对象，执行方法，但不能对非public的类型执行setAccessible(true)。
- 如果一个package是open的，我们不可以在其他modules 直接声明对象，但可以在其他modules中通过反射创建对象，执行方法，也可以对非public的类型执行setAccessible(true)。
  open可以修辞module，也可以是package，并允许限定范围。通过open关键字，像spring这样的DI框架就可以很容易地注入实现类。通过uses和providers…with，ServiceLoader可以实现相同的功能。
- 需要容器创建的bean，需要exports to spring.beans, 有的还需要spring.content，暂不清楚；
- 包含注入的属性的bean，需要opens to spring.core。
- controller需要exports to spring.web。
- Aspect相关的需要 exports to aop
- 还有那些需要反射的场景

那些在resources中的配置等内容，也需要将相关目录open，可参考classes下的结构，否则无法被外界访问，最容易漏掉的就是config中的spring boot配置。

应该是有相关插件，用于生成module-info.java文件的。

- [Project Jigsaw](http://openjdk.java.net/projects/jigsaw/quick-start)
- [warn: requires transitive directive for an automatic module](https://stackoverflow.com/questions/49600947/how-to-suppress-the-requires-transitive-directive-for-an-automatic-module-warn)
- [jmod-example](https://github.com/khmarbaise/jdk9-jlink-jmod-example)

迁移中遇到的一些问题

- [module xxx read package xxx from both xxx and xxx ](https://stackoverflow.com/questions/44697738/how-to-resolve-module-reads-package-error-in-java9)
- [java-9-migration-guide 强烈推荐](https://nipafx.dev/java-9-migration-guide/)

基于下面这俩链接，基本说明了在下一个大版本，将对JPMS进行支持

- [Spring Boot Java 9+ modularity](https://github.com/spring-projects/spring-boot/issues/13799)
- [Declare Spring modules with JDK 9 module metadata [SPR-13501]](https://github.com/spring-projects/spring-framework/issues/18079)

```shell
# 将无法模块化的放到clib中，可模块化的放到mlib，将未升级模块的依赖放入mlib中，会自动模块化，称为自动模块
# 当前待解决：模块 lwohvye.unicorn.starter 不具有 ModuleMainClass 属性，请使用 -m <模块>/<主类>。针对Spring Boot项目，应该有某个地方不一样
java -p mlib -Dloader.path=clib -m lwohvye.unicorn.starter
```

- ~~在Idea的 Run/Debug Configurations中的VM options中，部分可能需要调一下~~
- 直接启动报异常，是因为部分依赖无法module化（无法得到 module description，在编译时有相关警告
  can't extract module name from xxx.jar: Provider class xxx not in module）
  ，[具体原因](https://stackoverflow.com/questions/54682417/unable-to-derive-module-descriptor-provider-class-x-not-in-module)

```
Error occurred during initialization of boot layer
java.lang.module.FindException: Unable to derive module descriptor for xxx.jar
Caused by: java.lang.module.InvalidModuleDescriptorException: Provider class xxx.xxx.xxx not in module
```

- 经简单验证，若无无法module化的依赖，是可以启动成功的
- 另，若删除主启动类的module-info.java，以未命名模块的方式来运行，也是一种方式，这算是对Java 9之前的一个兼容
- 2022-07-06更新：
    - 打包报错在使用maven-jar-plugin从jar中剔除配置文件时出现，若不从中剔除则可正常打包
    - 基于加载规则，外置的配置项会覆盖内置的，所以对正常使用影响不大，一般active不同的profiles，且往往通过配置中心获取配置
    - ~~当前模块化部署未找到外置依赖的方式，使用 `-Dloader.path=lib` 会报类找不到~~
    - 打包可正常运行，虽似乎的确是模块化的方式（因该不是，这就更奇怪了，module-info.java也有打进去，等Spring Boot 3.x吧），但从内部获取Module却是unnamed module不是很理解，
      因为之前unnamed module应该需要加 `--add-opens java.base/java.lang=ALL-UNNAMED` （又试了一下，不加也行，忘了怎么配置导致的不需要加这个了）
- 2022-07-19更新：
    - 已外置依赖成功（推测是在maven-jar-plugin中配置了addClasspath，解决了之前的问题，classpath写在META-INF/MANIFEST.MF中）
- 2022-07-20更新：
    - 通过IDEA启动应该偏向于普通的project的加载执行方式，而打包部署时，则是Spring Boot Jar Package的加载执行方式，这俩是不一样的，
      When run through IDEA the Classloader is `jdk.internal.loader.ClassLoaders$AppClassLoader`,
      and When run through package-jar the Classloader is `org.springframework.boot.loader.LaunchedURLClassLoader`.
      When do package,spring-boot-loader and spring-boot-jarmode-layertools are added to the lib automatically.

IDEA中，两种启动方式的启动参数，另通过查看VM参数，module模式中有`--module-path`属性

```shell
# 非module
java -classpath lib com.lwohvye.AppSearchRun
# module
java -classpath lib -m lwohvye.unicorn.starter/com.lwohvye.AppRun
```

[SpringBoot jar包启动原理](https://www.lwohvye.com/2022/03/09/springboot-jar%e5%8c%85%e5%90%af%e5%8a%a8%e7%9a%84%e5%8e%9f%e7%90%86/)

#### Records

Enhance the Java programming language with records, which are classes that act as transparent carriers for immutable data. Records can be thought of as nominal
tuples.

- [JEP 395: Records](https://openjdk.java.net/jeps/395)
- 当前已知的几点，
    - Unsafe的部分方法不支持，例如：Unsafe: can't get field offset on a record class
    - 通过一些方式，可以对final属性进行修改，但record的似乎不行
- Motivation: It is a common complaint that "Java is too verbose" or has "too much ceremony".
- 初步判断，Record与原本的类存在一些差异，部分功能当前还未支持Record，白天进一步验证
- Record推出背后的目标是使开发人员能够将相关字段作为单个不可变数据项组合在一起，而不需要编写冗长的代码。这意味着，每当您想要向您的记录添加更多的字段/方法时，请考虑是否应该使用完整的类来代替它。
  (详见Goals and Non-Goals)
- Record类，本身属性是private final的，内部允许有其他属性，这些需要是static的(Instance field is not allowed in record)
- Record类的父类为java.lang.Record，所以不能再继承其他类，但可实现接口
- 之前基于业务需要，可能会用到元组tuple，这部分用 [Local Class](https://docs.oracle.com/javase/specs/jls/se14/html/jls-14.html#jls-14.3) 来做可能更好一些，对此record也支持local
  record class (A record class with components is clearer and safer than an anonymous tuple of implicitly params.)
- Like nested record classes, local record classes are implicitly static. The fact that local record classes are implicitly static is in contrast to local
  classes, which are not implicitly static. In fact, local classes are never static — implicitly or explicitly — and can always access variables in the
  enclosing method.

#### 附：

- 修改final属性的方式：
    - 非静态final属性，可以通过反射修改访问修饰符，将其改为非final的
  ```java
    public static void modifyFinalField(Object object, String fieldName, Object newFieldValue) throws Exception {
      Field field = object.getClass().getDeclaredField(fieldName);
      Field modifiersField = Field.class.getDeclaredField("modifiers"); // 获取Field的访问修饰符
      modifiersField.trySetAccessible(); //Field 的 modifiers 是私有的
      modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL); // 将Field的访问修饰符设置为非final的
      field.trySetAccessible();
      field.set(object, newFieldValue);
    }
  ```
    - 如果是静态final属性，同样可以先改为非final的（~~注意如果这里在去掉 final 之前就取了一次值,就会 set 失败, 因为 Class 默认开启了 useCaches 缓存,
      get 的时候会获取到 root field 的 FieldAccessor, 后面的重设就会失效，~~ 这一点没能验证）
    - 针对上面两种方式，可能代码执行下来没问题，但输出又还是原来的值？但总是可以通过反射方式获取到修改后的新值。这就是 Java 编译器对 final 属型的内联优化，即编译时把该
      final 的值直接放到了引用它的地方。即使是反射修改了该属性，引用的地方还是原值。Java对基本类型及Literal String 类型(直接双引号字符串)的final值会进行内联优化，
      而包装类型及通过new String("xx")创建的final值是不会被内联优化的，总之：只要不会被编译器内联优化的
      final 属性就可以通过反射有效的进行修改,修改后代码中可使用到新的值，另外如果 final 属性值是通过构造函数传入的也不会被编译器内联优化，所以能被有效的修改。
    - We can not change the static final fields by getAndChangeModifiers since JDK12.（java.lang.NoSuchFieldException: modifiers）。从Java 12开始已经不行咯，用下面的方式吧
    - 可以通过Unsafe的相关方法实现修改，这个是无视访问修饰符的 putXXX()
  ```java
    unsafe.putObject(obj, unsafe.objectFieldOffset(field), value); // 实例对象属性
    unsafe.putObject(unsafe.staticFieldBase(field), unsafe.staticFieldOffset(field), value); // 静态属性
  // 获取属性
     unsafe.getObject(obj, unsafe.objectFieldOffset(field));
     unsafe.getObject(unsafe.staticFieldBase(field), unsafe.staticFieldOffset(field));
  ```
    - 不推荐使用Unsafe，所以在1.7出现了MethodHandle，通过不同的lookup，获取到find系列方法，然后invoke系列执行，其与反射类似，但效率很高。
      牵涉到final，要用IMPL_LOOKUP(绕过一些检查)
    ```java
      implLookup.findSetter(field.getDeclaringClass(), field.getName(), field.getType()).invoke(obj, value);
      implLookup.findStaticSetter(field.getDeclaringClass(), field.getName(), field.getType()).invoke(value); // 这种可以，虽然注释似乎意思是不支持final的样子：if access checking fails, or if the field is not static or is final
  // 获取属性
      implLookup.findGetter(field.getDeclaringClass(),field.getName(),field.getType()).invoke(obj);
      implLookup.findStaticGetter(field.getDeclaringClass(), field.getName(), field.getType()).invoke();
    ```
    - 在1.9出现了VarHandle，通过find获取VarHandle，之后调用set()设置属性，**但static final是不支持的**。牵涉到final，也要用IMPL_LOOKUP
    ```java
       implLookup.findVarHandle(field.getDeclaringClass(), field.getName(), field.getType()).set(obj, value);
       implLookup.findStaticVarHandle(field.getDeclaringClass(), field.getName(), field.getType()).set(value); // 这种支持static但不支持final的，更细节的可以看findStaticVarHandle()上的注释
  // 获取属性
       implLookup.findVarHandle(field.getDeclaringClass(), field.getName(), field.getType()).get(obj);
       implLookup.findStaticVarHandle(field.getDeclaringClass(), field.getName(), field.getType()).get();
    ```
    - IMPL_LOOKUP当前只能通过Unsafe获取到
  ```java
    var unsafeField = Unsafe.class.getDeclaredField("theUnsafe");
    unsafeField.trySetAccessible();
    var unsafe = (Unsafe) unsafeField.get(null);
    // IMPL_LOOKUP 是用来判断私有方法是否被信任的标识，用来控制访问权限的.默认是false
    var implLookupField = MethodHandles.Lookup.class.getDeclaredField("IMPL_LOOKUP");
    // implLookupField.trySetAccessible();
    // 当前这里只能通过Unsafe来获取，后续再试试其他的获取方式，比如被注释的通过反射获取的，既然有人这样写，理论上是可行的，只是某些条件不满足
    implLookup =
         // (MethodHandles.Lookup) implLookupField.get(null); 这种方式获取不到，因为上面的trySetAccessible()会返回false表示设置失败，所以无法这样获取值
         (MethodHandles.Lookup) unsafe.getObject(unsafe.staticFieldBase(implLookupField), unsafe.staticFieldOffset(implLookupField));
  ```
    - 
