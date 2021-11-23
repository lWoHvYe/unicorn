#!/bin/bash
echo "stop SpringBoot Application"
# shellcheck disable=SC2009
pid=$(ps -ef | grep eladmin-system-2.6.17.jar | grep -v grep | awk '{print $2}')
echo "旧应用进程id：$pid"
if [ -n "$pid" ]
then
# 通过使用-15 而不是-9 来停止线程
kill -15 "$pid"
fi
