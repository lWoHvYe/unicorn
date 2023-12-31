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

echo "stop SpringBoot Application"
# shellcheck disable=SC2009
ps -ef | grep valentine-starter.jar | grep -v grep | awk '{print $2}' | xargs kill -15
#pid=$(ps -ef | grep valentine-starter.jar | grep -v grep | awk '{print $2}')
#echo "旧应用进程id：$pid"
#if [ -n "$pid" ]
#then
# 通过使用-15 而不是-9 来停止线程
#kill -15 "$pid"
#fi
