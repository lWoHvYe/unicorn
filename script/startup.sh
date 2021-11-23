#!/bin/bash
export JAVA_HOME=/usr/java/jdk-17
echo ${JAVA_HOME}
# ''单引号原样输出。""双引号可调用变量输出
echo '切换到工作目录'
# shellcheck disable=SC2164
cd /opt/app
echo '授权当前用户'
chmod 555 ./*.jar
echo '执行....'
nohup ${JAVA_HOME}/bin/java -Xms100m -Xmx500m --add-opens java.base/java.lang=ALL-UNNAMED -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:5005 -jar eladmin-system-3.0.0.jar >elog.out 2>&1 &
echo '启动成功'
