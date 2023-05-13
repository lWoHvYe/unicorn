- put the tomcat-coyote-loom.jar (the jar is build [from](https://github.com/apache/tomcat/tree/main/modules/loom) ) to
  the `lib` folder of tomcat
- update server.xml

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Server port="8005" shutdown="SHUTDOWN">
    <Listener className="org.apache.catalina.startup.VersionLoggerListener"/>

    <Service name="Catalina">

        <!--The connectors can use a shared executor, you can define one or more named thread pools-->
        <!--
        <Executor name="tomcatThreadPool" namePrefix="catalina-exec-"
            maxThreads="150" minSpareThreads="4"/>
        -->
        <Executor name="loomExecutor"
                  className="org.apache.catalina.core.LoomExecutor"/>


        <!-- A "Connector" using the shared thread pool-->
        <!--
        <Connector executor="tomcatThreadPool"
                   port="8080" protocol="HTTP/1.1"
                   connectionTimeout="20000"
                   redirectPort="8443" />
        -->
        <Connector
                executor="loomExecutor"
                port="8080" protocol="org.apache.coyote.http11.Http11NioProtocol"
                connectionTimeout="20000"
                redirectPort="8443"/>

        <Engine name="Catalina" defaultHost="localhost">
            <Host name="localhost" appBase="webapps"
                  unpackWARs="true" autoDeploy="true">
            </Host>
        </Engine>
    </Service>
</Server>
```

- for Java 19/20, add setenv.sh in `bin` folder，for Java 21 and later, this is not necessary.

```shell
#!/bin/sh
JAVA_OPTS=--enable-preview
```
