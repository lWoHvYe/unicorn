FROM openjdk:18.0.2

WORKDIR /unicorn_app
ENV TZ=Asia/Shanghai

COPY valentine-starter/target/lib/* /unicorn_app/ex_lib/
COPY valentine-starter/target/config/* /unicorn_app/config/
ADD valentine-starter/target/valentine-starter.*.jar /unicorn_app/valentine-starter.jar

EXPOSE 8082
ENTRYPOINT ["java", "-Dloader.path=ex_lib", "-jar", "unicorn-starter.jar"]
