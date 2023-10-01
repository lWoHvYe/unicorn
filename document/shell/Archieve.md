后台运行jar（开启远程调试端口5005）。2>&1 表示在同一个文件中同时捕获 System.err和
System.out（有一个箭头的表示以覆盖的方式重定向，而有两个箭头的表示以追加的方式重定向。如果需要将标准输出以及标准错误输出同时重定向到一个文件，需要将某个输出转换为另一个输出，例如
2>&1 表示将标准错误输出转换为标准输出）。

```shell
nohup java -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005 -jar unicorn-starter-3.2.0.jar >nohup.out 2>&1 &
```

若外置依赖启动参数需添加，``-Dloader.path=lib``引入依赖。外置依赖可以大大减少jar包的体积。方便后续更新部署

```shell
#2.x版本启动示例
nohup java --add-opens java.base/java.lang=ALL-UNNAMED -Dloader.path=lib -jar unicorn-starter-2.6.18.jar >nohup.out 2>&1 &
```

```shell
#3.x版本开始，因为已完成JPMS改造，可移除启动参数中 --add-opens 部分
nohup java -XX:+UseZGC -Dloader.path=lib -jar unicorn-starter-3.2.0.jar >nohup.out 2>&1 &
```
