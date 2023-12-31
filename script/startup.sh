#!/bin/bash
#
#    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.
#

export JAVA_HOME=/usr/java/jdk-21
echo ${JAVA_HOME}
# ''单引号原样输出。""双引号可调用变量输出
echo '切换到工作目录'
# shellcheck disable=SC2164
cd /opt/app
echo '授权当前用户'
chmod 555 ./*.jar
echo '执行....'
nohup ${JAVA_HOME}/bin/java -Xms500m -Xmx1024m --enable-preview -XX:+UseZGC \
  -jar valentine-starter.jar >elog.out 2>&1 &
echo '启动成功'
