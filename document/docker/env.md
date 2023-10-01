#### RabbitMQ镜像集群

```shell
# 加入集群。先拷贝.erlang.cookie到各节点，然后在要加入的各节点上执行
# 停止服务
rabbitmqctl stop_app
# 重置状态
rabbitmqctl reset
# 加入节点
rabbitmqctl join_cluster rabbit@6b85193b51cb
# 启动服务
rabbitmqctl start_app

# 在把节点都加入后，可以开启镜像。也可以不开启，就是普通集群模式，开启了就是镜像集群模式。这里对满足正则的Queue做了镜像，且设置复制系数，只需同步到两个节点即可（半写原则 n/2 + 1）
rabbitmqctl set_policy ha-half -p /unicorn "^data\.|sp-sync" '{"ha-mode":"exactly","ha-params":2,"ha-sync-mode":"automatic"}'
```

#### MySQL集群

```shell
change master to master_host='mysql',master_user='slave',master_password='123456',master_port=3306,master_log_file='replicas-mysql-bin.000001',master_log_pos=156,master_connect_retry=30,get_master_public_key=1;
```
