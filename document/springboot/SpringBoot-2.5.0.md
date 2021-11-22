在Spring Boot 2.5版本存在报错(在2.5.1已修复)：（使用Idea时正常，jar运行时报错）

java.lang.IllegalStateException: No subdirectories found for mandatory directory location 'file:./config/*/'

解决方式为添加启动参数    --spring.config.location=optional:classpath:/,optional:classpath:/config/,optional:file:./,optional:file:./config/

参考：https://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#features.external-config.files

