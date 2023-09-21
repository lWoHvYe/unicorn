```shell
change master to master_host='mysql',master_user='slave',master_password='123456',master_port=3306,master_log_file='replicas-mysql-bin.000001',master_log_pos=156,master_connect_retry=30,get_master_public_key=1;
```
在 MySQL 数据库中，初始化主库和从库，以及进行主从复制的配置需要多个步骤。以下是一个一般性的步骤列表，以帮助您初始化主库和从库，以及在它们之间建立复制：

**在主库上的步骤：**

1. **安装 MySQL**：首先，在主机上安装 MySQL 数据库服务器。确保 MySQL 在主库上正常运行。

2. **配置主库**：在 MySQL 配置文件 (`my.cnf` 或 `my.ini`) 中，启用二进制日志文件 (`log-bin`) 和配置一个唯一的服务器 ID (`server-id`)。例如：

    ```ini
    server-id = 1
    log-bin = /var/log/mysql/mysql-bin.log
    ```

   还要确保启用了二进制日志文件的记录 (`binlog_format = ROW`)，以便记录更多详细的信息。

3. **创建复制用户**：为从库创建一个用于复制的用户，并授予适当的权限。

    ```sql
    CREATE USER 'replication_user'@'%' IDENTIFIED BY 'password';
    GRANT REPLICATION SLAVE ON *.* TO 'replication_user'@'%';
    ```

4. **锁定表并获取主库当前状态**：在导出数据之前，可以锁定表以确保一致性。然后，获取主库的当前状态。

    ```sql
    FLUSH TABLES WITH READ LOCK;
    SHOW MASTER STATUS;
    ```

   记下 `File` 和 `Position` 的值，它们将在从库上使用。

5. **导出数据**：使用 `mysqldump` 或其他方法导出主库的数据，并将其复制到从库。

6. **解锁表**：解锁表以恢复写操作。

    ```sql
    UNLOCK TABLES;
    ```

**在从库上的步骤：**

1. **安装 MySQL**：在从机上安装 MySQL 数据库服务器。确保 MySQL 在从库上正常运行。

2. **配置从库**：在 MySQL 配置文件 (`my.cnf` 或 `my.ini`) 中，配置一个唯一的服务器 ID (`server-id`)。

    ```ini
    server-id = 2
    ```

3. **导入数据**：将从主库复制的数据导入从库。您可以使用 `mysql` 命令行或其他导入工具。

4. **配置复制**：连接到主库并设置从库复制配置。使用以下命令：

    ```sql
    CHANGE MASTER TO
      MASTER_HOST = '主库的主机名或 IP 地址',
      MASTER_USER = 'replication_user',
      MASTER_PASSWORD = 'password',
      MASTER_LOG_FILE = '主库的二进制日志文件名',
      MASTER_LOG_POS = 主库的二进制日志位置;
    ```

   替换上述命令中的参数为主库的实际值。

5. **启动复制**：启动从库复制过程。

    ```sql
    START SLAVE;
    ```

6. **检查复制状态**：使用以下命令检查从库的复制状态。

    ```sql
    SHOW SLAVE STATUS\G
    ```

   确保状态中的 `Slave_IO_Running` 和 `Slave_SQL_Running` 均为 `Yes`。

这些步骤提供了初始化主库和从库，并在它们之间建立复制的一般指南。确保在实际操作中参考 MySQL 文档和最佳实践，以确保数据一致性和可用性。
