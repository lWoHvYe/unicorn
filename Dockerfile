FROM openjdk:18.0.2

MAINTAINER Hongyan, Wang

WORKDIR /unicorn_app
ENV TZ=Asia/Shanghai

COPY valentine-starter/target/lib/* ex_lib/
COPY valentine-starter/target/config/* config/
ADD valentine-starter/target/valentine-starter-*.jar valentine-starter.jar

EXPOSE 8082
ENTRYPOINT exec java -Dloader.path=ex_lib -jar valentine-starter.jar
