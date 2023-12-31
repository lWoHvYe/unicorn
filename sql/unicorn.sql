/*
 Navicat Premium Data Transfer

 Source Server         : 145-MySQL
 Source Server Type    : MySQL
 Source Server Version : 80026
 Source Host           : 10.211.55.145:3306
 Source Schema         : unicorn

 Target Server Type    : MySQL
 Target Server Version : 80026
 File Encoding         : 65001

 Date: 03/08/2023 08:37:40
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for code_column_config
-- ----------------------------
DROP TABLE IF EXISTS `code_column_config`;
CREATE TABLE `code_column_config`
(
    `column_id`       bigint NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `table_name`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `column_name`     varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `column_type`     varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `dict_name`       varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `extra`           varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `form_show`       bit(1)                                                  DEFAULT NULL,
    `form_type`       varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `key_type`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `list_show`       bit(1)                                                  DEFAULT NULL,
    `not_null`        bit(1)                                                  DEFAULT NULL,
    `query_type`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `remark`          varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `date_annotation` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    PRIMARY KEY (`column_id`) USING BTREE,
    KEY `idx_table_name` (`table_name`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 215
  DEFAULT CHARSET = utf8mb3
  ROW_FORMAT = COMPACT COMMENT ='代码生成字段信息存储';

-- ----------------------------
-- Records of code_column_config
-- ----------------------------
BEGIN;
INSERT INTO `code_column_config`
VALUES (191, 'sys_user', 'user_id', 'bigint', NULL, 'auto_increment', b'1', NULL, 'PRI', b'1', b'0', NULL, 'ID', NULL);
INSERT INTO `code_column_config`
VALUES (192, 'sys_user', 'dept_id', 'bigint', NULL, '', b'1', NULL, 'MUL', b'1', b'0', NULL, '部门名称', NULL);
INSERT INTO `code_column_config`
VALUES (193, 'sys_user', 'username', 'varchar', NULL, '', b'1', NULL, 'UNI', b'1', b'0', NULL, '用户名', NULL);
INSERT INTO `code_column_config`
VALUES (194, 'sys_user', 'nick_name', 'varchar', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '昵称', NULL);
INSERT INTO `code_column_config`
VALUES (195, 'sys_user', 'gender', 'varchar', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '性别', NULL);
INSERT INTO `code_column_config`
VALUES (196, 'sys_user', 'phone', 'varchar', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '手机号码', NULL);
INSERT INTO `code_column_config`
VALUES (197, 'sys_user', 'email', 'varchar', NULL, '', b'1', NULL, 'UNI', b'1', b'0', NULL, '邮箱', NULL);
INSERT INTO `code_column_config`
VALUES (198, 'sys_user', 'avatar_name', 'varchar', NULL, '', b'1', NULL, 'MUL', b'1', b'0', NULL, '头像地址', NULL);
INSERT INTO `code_column_config`
VALUES (199, 'sys_user', 'avatar_path', 'varchar', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '头像真实路径', NULL);
INSERT INTO `code_column_config`
VALUES (200, 'sys_user', 'password', 'varchar', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '密码', NULL);
INSERT INTO `code_column_config`
VALUES (201, 'sys_user', 'is_admin', 'bit', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '是否为admin账号', NULL);
INSERT INTO `code_column_config`
VALUES (202, 'sys_user', 'enabled', 'bigint', NULL, '', b'1', NULL, 'MUL', b'1', b'0', NULL, '状态：1启用、0禁用', NULL);
INSERT INTO `code_column_config`
VALUES (203, 'sys_user', 'description', 'mediumblob', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '描述信息', NULL);
INSERT INTO `code_column_config`
VALUES (204, 'sys_user', 'create_by', 'varchar', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '创建者', NULL);
INSERT INTO `code_column_config`
VALUES (205, 'sys_user', 'update_by', 'varchar', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '更新者', NULL);
INSERT INTO `code_column_config`
VALUES (206, 'sys_user', 'pwd_reset_time', 'datetime', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '修改密码的时间',
        NULL);
INSERT INTO `code_column_config`
VALUES (207, 'sys_user', 'create_time', 'datetime', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '创建日期', NULL);
INSERT INTO `code_column_config`
VALUES (208, 'sys_user', 'update_time', 'datetime', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '更新时间', NULL);
INSERT INTO `code_column_config`
VALUES (209, 'sys_resource', 'resource_id', 'bigint', NULL, '', b'1', NULL, 'PRI', b'1', b'1', NULL, 'ID', NULL);
INSERT INTO `code_column_config`
VALUES (210, 'sys_resource', 'name', 'varchar', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '资源名称', NULL);
INSERT INTO `code_column_config`
VALUES (211, 'sys_resource', 'pattern', 'varchar', NULL, '', b'1', NULL, 'MUL', b'1', b'1', NULL, 'URI', NULL);
INSERT INTO `code_column_config`
VALUES (212, 'sys_resource', 'status', 'tinyint', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '状态 0-不可用 1-可用',
        NULL);
INSERT INTO `code_column_config`
VALUES (213, 'sys_resource', 'rest_name', 'varchar', NULL, '', b'1', NULL, 'MUL', b'1', b'0', NULL, '所在类名', NULL);
INSERT INTO `code_column_config`
VALUES (214, 'sys_resource', 'remark', 'varchar', NULL, '', b'1', NULL, '', b'1', b'0', NULL, '备注', NULL);
COMMIT;

-- ----------------------------
-- Table structure for code_gen_config
-- ----------------------------
DROP TABLE IF EXISTS `code_gen_config`;
CREATE TABLE `code_gen_config`
(
    `config_id`   bigint NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `table_name`  varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '表名',
    `author`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '作者',
    `cover`       bit(1)                                                  DEFAULT NULL COMMENT '是否覆盖',
    `module_name` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '模块名称',
    `pack`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '至于哪个包下',
    `path`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '前端代码生成的路径',
    `api_path`    varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '前端Api文件路径',
    `prefix`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '表前缀',
    `api_alias`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '接口名称',
    PRIMARY KEY (`config_id`) USING BTREE,
    KEY `idx_table_name` (`table_name`(100)) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb3
  ROW_FORMAT = COMPACT COMMENT ='代码生成器配置';

-- ----------------------------
-- Records of code_gen_config
-- ----------------------------
BEGIN;
COMMIT;

-- ----------------------------
-- Table structure for oauth2_authorization
-- ----------------------------
DROP TABLE IF EXISTS `oauth2_authorization`;
CREATE TABLE `oauth2_authorization`
(
    `id`                            varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
    `registered_client_id`          varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
    `principal_name`                varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
    `authorization_grant_type`      varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
    `authorized_scopes`             varchar(1000) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci     DEFAULT NULL,
    `attributes`                    blob,
    `state`                         varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci      DEFAULT NULL,
    `authorization_code_value`      blob,
    `authorization_code_issued_at`  timestamp                                                     NULL DEFAULT NULL,
    `authorization_code_expires_at` timestamp                                                     NULL DEFAULT NULL,
    `authorization_code_metadata`   blob,
    `access_token_value`            blob,
    `access_token_issued_at`        timestamp                                                     NULL DEFAULT NULL,
    `access_token_expires_at`       timestamp                                                     NULL DEFAULT NULL,
    `access_token_metadata`         blob,
    `access_token_type`             varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci      DEFAULT NULL,
    `access_token_scopes`           varchar(1000) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci     DEFAULT NULL,
    `oidc_id_token_value`           blob,
    `oidc_id_token_issued_at`       timestamp                                                     NULL DEFAULT NULL,
    `oidc_id_token_expires_at`      timestamp                                                     NULL DEFAULT NULL,
    `oidc_id_token_metadata`        blob,
    `oidc_id_token_claims`          blob,
    `refresh_token_value`           blob,
    `refresh_token_issued_at`       timestamp                                                     NULL DEFAULT NULL,
    `refresh_token_expires_at`      timestamp                                                     NULL DEFAULT NULL,
    `refresh_token_metadata`        blob,
    `user_code_value`               blob,
    `user_code_issued_at`           timestamp                                                     NULL DEFAULT NULL,
    `user_code_expires_at`          timestamp                                                     NULL DEFAULT NULL,
    `user_code_metadata`            blob,
    `device_code_value`             blob,
    `device_code_issued_at`         timestamp                                                     NULL DEFAULT NULL,
    `device_code_expires_at`        timestamp                                                     NULL DEFAULT NULL,
    `device_code_metadata`          blob,
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci
  ROW_FORMAT = COMPACT;

-- ----------------------------
-- Records of oauth2_authorization
-- ----------------------------
BEGIN;
COMMIT;

-- ----------------------------
-- Table structure for oauth2_authorization_consent
-- ----------------------------
DROP TABLE IF EXISTS `oauth2_authorization_consent`;
CREATE TABLE `oauth2_authorization_consent`
(
    `registered_client_id` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci  NOT NULL,
    `principal_name`       varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci  NOT NULL,
    `authorities`          varchar(1000) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
    PRIMARY KEY (`registered_client_id`, `principal_name`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci;

-- ----------------------------
-- Records of oauth2_authorization_consent
-- ----------------------------
BEGIN;
COMMIT;

-- ----------------------------
-- Table structure for oauth2_registered_client
-- ----------------------------
DROP TABLE IF EXISTS `oauth2_registered_client`;
CREATE TABLE `oauth2_registered_client`
(
    `id`                            varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci  NOT NULL,
    `client_id`                     varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci  NOT NULL,
    `client_id_issued_at`           timestamp                                                      NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `client_secret`                 varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci           DEFAULT NULL,
    `client_secret_expires_at`      timestamp                                                      NULL     DEFAULT NULL,
    `client_name`                   varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci  NOT NULL,
    `client_authentication_methods` varchar(1000) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
    `authorization_grant_types`     varchar(1000) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
    `redirect_uris`                 varchar(1000) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci          DEFAULT NULL,
    `scopes`                        varchar(1000) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
    `client_settings`               varchar(2000) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
    `token_settings`                varchar(2000) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
    `post_logout_redirect_uris`     varchar(1000) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci          DEFAULT NULL,
    PRIMARY KEY (`id`),
    KEY `client_id` (`client_id`),
    KEY `client_name` (`client_name`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci;

-- ----------------------------
-- Records of oauth2_registered_client
-- ----------------------------
BEGIN;
INSERT INTO `oauth2_registered_client`
VALUES ('74b93558-77bf-4b6e-8ee6-d4f262fce099', 'login-client', '2023-04-05 21:23:58',
        '$2a$10$9ld5pThtd0BtivV7aWF1vOeiAB5havfmjz8d/g9MVTHnUVzHYy0Za', NULL, '74b93558-77bf-4b6e-8ee6-d4f262fce099',
        'client_secret_basic', 'refresh_token,authorization_code',
        'http://127.0.0.1:8080/login/oauth2/code/login-client', 'openid,resource.read,profile,email,resource.write',
        '{\"@class\":\"java.util.Collections$UnmodifiableMap\",\"settings.client.require-proof-key\":false,\"settings.client.require-authorization-consent\":true}',
        '{\"@class\":\"java.util.Collections$UnmodifiableMap\",\"settings.token.reuse-refresh-tokens\":true,\"settings.token.id-token-signature-algorithm\":[\"org.springframework.security.oauth2.jose.jws.SignatureAlgorithm\",\"RS256\"],\"settings.token.authorization-code-time-to-live\":[\"java.time.Duration\",300.000000000],\"settings.token.access-token-time-to-live\":[\"java.time.Duration\",300.000000000],\"settings.token.access-token-format\":{\"@class\":\"org.springframework.security.oauth2.server.authorization.settings.OAuth2TokenFormat\",\"value\":\"self-contained\"},\"settings.token.refresh-token-time-to-live\":[\"java.time.Duration\",3600.000000000]}',
        NULL);
INSERT INTO `oauth2_registered_client`
VALUES ('87d1cc25-772b-4de5-b523-eb65a3af0264', 'messaging-client', '2023-04-05 21:24:03',
        '$2a$10$LS.I40Br17yt0sYZxKiyqe5QPYBUZIvhkKyKnmRTmUT1nHdC07jmG', NULL, '87d1cc25-772b-4de5-b523-eb65a3af0264',
        'client_secret_basic', 'refresh_token,client_credentials,authorization_code',
        'http://127.0.0.1:8080/authorized,http://127.0.0.1:8080/login/oauth2/code/messaging-client-oidc',
        'openid,resource.read,profile,message.read,resource.write,message.write',
        '{\"@class\":\"java.util.Collections$UnmodifiableMap\",\"settings.client.require-proof-key\":false,\"settings.client.require-authorization-consent\":true}',
        '{\"@class\":\"java.util.Collections$UnmodifiableMap\",\"settings.token.reuse-refresh-tokens\":true,\"settings.token.id-token-signature-algorithm\":[\"org.springframework.security.oauth2.jose.jws.SignatureAlgorithm\",\"RS256\"],\"settings.token.authorization-code-time-to-live\":[\"java.time.Duration\",300.000000000],\"settings.token.access-token-time-to-live\":[\"java.time.Duration\",300.000000000],\"settings.token.access-token-format\":{\"@class\":\"org.springframework.security.oauth2.server.authorization.settings.OAuth2TokenFormat\",\"value\":\"self-contained\"},\"settings.token.refresh-token-time-to-live\":[\"java.time.Duration\",3600.000000000]}',
        NULL);
COMMIT;

-- ----------------------------
-- Table structure for sys_dept
-- ----------------------------
DROP TABLE IF EXISTS `sys_dept`;
CREATE TABLE `sys_dept`
(
    `dept_id`     bigint                                                  NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `pid`         bigint                                                  DEFAULT NULL COMMENT '上级部门',
    `sub_count`   int                                                     DEFAULT '0' COMMENT '子部门数目',
    `name`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '名称',
    `dept_sort`   int                                                     DEFAULT '999' COMMENT '排序',
    `enabled`     bit(1)                                                  NOT NULL COMMENT '状态',
    `create_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建者',
    `update_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '更新者',
    `create_time` datetime                                                DEFAULT NULL COMMENT '创建日期',
    `update_time` datetime                                                DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`dept_id`) USING BTREE,
    KEY `inx_pid` (`pid`) USING BTREE,
    KEY `inx_enabled` (`enabled`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 25
  DEFAULT CHARSET = utf8mb3 COMMENT ='部门';

-- ----------------------------
-- Records of sys_dept
-- ----------------------------
BEGIN;
INSERT INTO `sys_dept`
VALUES (2, 7, 2, '研发部', 3, b'1', 'admin', 'admin', '2019-03-25 09:15:32', '2020-08-02 14:48:47');
INSERT INTO `sys_dept`
VALUES (5, 7, 0, '运维部', 4, b'1', 'admin', 'admin', '2019-03-25 09:20:44', '2020-05-17 14:27:27');
INSERT INTO `sys_dept`
VALUES (6, 8, 0, '测试部', 6, b'1', 'admin', 'admin', '2019-03-25 09:52:18', '2020-06-08 11:59:21');
INSERT INTO `sys_dept`
VALUES (7, NULL, 2, '华南分部', 0, b'1', 'admin', 'admin', '2019-03-25 11:04:50', '2020-06-08 12:08:56');
INSERT INTO `sys_dept`
VALUES (8, NULL, 4, '华北分部', 1, b'1', 'admin', 'admin', '2019-03-25 11:04:53', '2020-05-14 12:54:00');
INSERT INTO `sys_dept`
VALUES (15, 8, 0, 'UI部门', 7, b'1', 'admin', 'admin', '2020-05-13 22:56:53', '2020-05-14 12:54:13');
INSERT INTO `sys_dept`
VALUES (18, 2, 0, '平台开发部', 2, b'1', 'admin', 'admin', '2021-10-23 23:37:25', '2021-10-23 23:37:25');
INSERT INTO `sys_dept`
VALUES (19, 2, 0, '产品开发部', 4, b'1', 'admin', 'admin', '2021-10-23 23:38:03', '2021-10-23 23:38:03');
INSERT INTO `sys_dept`
VALUES (20, 8, 0, '运营部', 8, b'1', 'admin', 'admin', '2021-11-05 09:57:26', '2021-11-05 09:57:42');
INSERT INTO `sys_dept`
VALUES (21, NULL, 2, '华中分部', 7, b'1', 'admin', 'admin', '2021-11-24 18:47:03', '2021-11-24 18:47:03');
INSERT INTO `sys_dept`
VALUES (22, 21, 0, '市场部', 1, b'1', 'admin', 'admin', '2021-11-24 18:47:22', '2021-11-24 18:47:31');
INSERT INTO `sys_dept`
VALUES (23, 21, 0, '支撑部', 2, b'1', 'admin', 'admin', '2021-11-24 18:47:54', '2022-03-13 21:56:40');
INSERT INTO `sys_dept`
VALUES (24, 8, 0, '祭祀部', 8, b'1', 'admin', 'admin', '2022-09-11 19:03:58', '2022-09-11 19:03:58');
COMMIT;

-- ----------------------------
-- Table structure for sys_dict
-- ----------------------------
DROP TABLE IF EXISTS `sys_dict`;
CREATE TABLE `sys_dict`
(
    `dict_id`     bigint                                                  NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `name`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '字典名称',
    `description` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '描述',
    `create_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建者',
    `update_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '更新者',
    `create_time` datetime                                                DEFAULT NULL COMMENT '创建日期',
    `update_time` datetime                                                DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`dict_id`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 7
  DEFAULT CHARSET = utf8mb3 COMMENT ='数据字典';

-- ----------------------------
-- Records of sys_dict
-- ----------------------------
BEGIN;
INSERT INTO `sys_dict`
VALUES (1, 'status', '状态', NULL, 'admin', '2019-10-27 20:31:36', '2023-06-16 20:58:40');
INSERT INTO `sys_dict`
VALUES (4, 'dept_status', '部门状态', NULL, NULL, '2019-10-27 20:31:36', NULL);
INSERT INTO `sys_dict`
VALUES (5, 'job_status', '岗位状态', NULL, NULL, '2019-10-27 20:31:36', NULL);
INSERT INTO `sys_dict`
VALUES (6, 'Sys_Type', '类型信息', 'admin', 'admin', '2022-09-11 18:42:31', '2022-09-11 18:42:31');
COMMIT;

-- ----------------------------
-- Table structure for sys_dict_detail
-- ----------------------------
DROP TABLE IF EXISTS `sys_dict_detail`;
CREATE TABLE `sys_dict_detail`
(
    `detail_id`   bigint                                                  NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `dict_id`     bigint                                                  DEFAULT NULL COMMENT '字典id',
    `label`       varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '字典标签',
    `value`       varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '字典值',
    `dict_sort`   int                                                     DEFAULT NULL COMMENT '排序',
    `create_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建者',
    `update_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '更新者',
    `create_time` datetime                                                DEFAULT NULL COMMENT '创建日期',
    `update_time` datetime                                                DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`detail_id`) USING BTREE,
    KEY `FK5tpkputc6d9nboxojdbgnpmyb` (`dict_id`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 10
  DEFAULT CHARSET = utf8mb3 COMMENT ='数据字典详情';

-- ----------------------------
-- Records of sys_dict_detail
-- ----------------------------
BEGIN;
INSERT INTO `sys_dict_detail`
VALUES (1, 1, '激活', 'true', 1, NULL, NULL, '2019-10-27 20:31:36', NULL);
INSERT INTO `sys_dict_detail`
VALUES (2, 1, '禁用', 'false', 2, NULL, NULL, NULL, NULL);
INSERT INTO `sys_dict_detail`
VALUES (3, 4, '启用', 'true', 1, NULL, NULL, NULL, NULL);
INSERT INTO `sys_dict_detail`
VALUES (4, 4, '停用', 'false', 2, NULL, NULL, '2019-10-27 20:31:36', NULL);
INSERT INTO `sys_dict_detail`
VALUES (5, 5, '启用', 'true', 1, NULL, NULL, NULL, NULL);
INSERT INTO `sys_dict_detail`
VALUES (6, 5, '停用', 'false', 2, NULL, NULL, '2019-10-27 20:31:36', NULL);
INSERT INTO `sys_dict_detail`
VALUES (7, 6, 'Unicorn', 'unicorn', 1, 'admin', 'admin', '2022-09-11 18:53:58', '2022-09-11 18:53:58');
INSERT INTO `sys_dict_detail`
VALUES (8, 6, 'Valentine', 'valentine', 2, 'admin', 'admin', '2022-09-11 18:54:27', '2022-09-11 18:54:27');
INSERT INTO `sys_dict_detail`
VALUES (9, 6, 'Enterprise', 'enterprise', 3, 'admin', 'admin', '2022-09-11 18:54:48', '2022-09-11 18:54:48');
COMMIT;

-- ----------------------------
-- Table structure for sys_job
-- ----------------------------
DROP TABLE IF EXISTS `sys_job`;
CREATE TABLE `sys_job`
(
    `job_id`      bigint                                                  NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `name`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '岗位名称',
    `enabled`     bit(1)                                                  NOT NULL COMMENT '岗位状态',
    `job_sort`    int                                                     DEFAULT NULL COMMENT '排序',
    `create_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建者',
    `update_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '更新者',
    `create_time` datetime                                                DEFAULT NULL COMMENT '创建日期',
    `update_time` datetime                                                DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`job_id`) USING BTREE,
    UNIQUE KEY `uniq_name` (`name`) USING BTREE,
    KEY `inx_enabled` (`enabled`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 16
  DEFAULT CHARSET = utf8mb3 COMMENT ='岗位';

-- ----------------------------
-- Records of sys_job
-- ----------------------------
BEGIN;
INSERT INTO `sys_job`
VALUES (8, '人事专员', b'1', 3, NULL, NULL, '2019-03-29 14:52:28', NULL);
INSERT INTO `sys_job`
VALUES (10, '产品经理', b'1', 4, NULL, NULL, '2019-03-29 14:55:51', NULL);
INSERT INTO `sys_job`
VALUES (11, '全栈开发', b'1', 2, NULL, 'admin', '2019-03-31 13:39:30', '2020-05-05 11:33:43');
INSERT INTO `sys_job`
VALUES (12, '软件测试', b'1', 5, NULL, 'admin', '2019-03-31 13:39:43', '2020-05-10 19:56:26');
INSERT INTO `sys_job`
VALUES (13, '服务端开发', b'1', 8, 'admin', 'admin', '2021-11-05 09:56:17', '2021-11-24 18:46:02');
INSERT INTO `sys_job`
VALUES (14, '产品端开发', b'1', 9, 'admin', 'admin', '2021-11-05 09:56:43', '2021-11-05 09:56:43');
INSERT INTO `sys_job`
VALUES (15, '产品运营', b'1', 6, 'admin', 'admin', '2021-11-05 22:39:20', '2021-11-05 22:39:20');
COMMIT;

-- ----------------------------
-- Table structure for sys_log
-- ----------------------------
DROP TABLE IF EXISTS `sys_log`;
CREATE TABLE `sys_log`
(
    `log_id`           bigint NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `description`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `log_type`         varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `method`           varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `params`           mediumblob,
    `request_ip`       varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `time`             bigint                                                  DEFAULT NULL,
    `username`         varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `address`          varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `browser`          varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `exception_detail` text CHARACTER SET utf8 COLLATE utf8_general_ci,
    `create_time`      datetime                                                DEFAULT NULL,
    PRIMARY KEY (`log_id`) USING BTREE,
    KEY `log_create_time_index` (`create_time`) USING BTREE,
    KEY `inx_log_type` (`log_type`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 3898
  DEFAULT CHARSET = utf8mb3 COMMENT ='系统日志';

-- ----------------------------
-- Records of sys_log
-- ----------------------------
BEGIN;
COMMIT;

-- ----------------------------
-- Table structure for sys_menu
-- ----------------------------
DROP TABLE IF EXISTS `sys_menu`;
CREATE TABLE `sys_menu`
(
    `menu_id`     bigint NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `pid`         bigint                                                  DEFAULT NULL COMMENT '上级菜单ID',
    `sub_count`   int                                                     DEFAULT '0' COMMENT '子菜单数目',
    `type`        int                                                     DEFAULT NULL COMMENT '菜单类型',
    `title`       varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '菜单标题',
    `name`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '组件名称',
    `component`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '组件',
    `menu_sort`   int                                                     DEFAULT NULL COMMENT '排序',
    `icon`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '图标',
    `path`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '链接地址',
    `i_frame`     bit(1)                                                  DEFAULT NULL COMMENT '是否外链',
    `cache`       bit(1)                                                  DEFAULT b'0' COMMENT '缓存',
    `hidden`      bit(1)                                                  DEFAULT b'0' COMMENT '隐藏',
    `permission`  varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '权限',
    `create_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建者',
    `update_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '更新者',
    `create_time` datetime                                                DEFAULT NULL COMMENT '创建日期',
    `update_time` datetime                                                DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`menu_id`) USING BTREE,
    UNIQUE KEY `uniq_title` (`title`) USING BTREE,
    UNIQUE KEY `uniq_name` (`name`) USING BTREE,
    KEY `inx_pid` (`pid`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 30117
  DEFAULT CHARSET = utf8mb3 COMMENT ='系统菜单';

-- ----------------------------
-- Records of sys_menu
-- ----------------------------
BEGIN;
INSERT INTO `sys_menu`
VALUES (1, NULL, 8, 0, '系统管理', NULL, NULL, 1, 'system', 'system', b'0', b'0', b'0', NULL, NULL, NULL,
        '2018-12-18 15:11:29', NULL);
INSERT INTO `sys_menu`
VALUES (2, 1, 3, 1, '用户管理', 'User', 'system/user/index', 2, 'peoples', 'user', b'0', b'0', b'0', 'user:list', NULL,
        NULL, '2018-12-18 15:14:44', NULL);
INSERT INTO `sys_menu`
VALUES (3, 1, 3, 1, '角色管理', 'Role', 'system/role/index', 3, 'role', 'role', b'0', b'0', b'0', 'roles:list', NULL,
        NULL, '2018-12-18 15:16:07', NULL);
INSERT INTO `sys_menu`
VALUES (5, 1, 3, 1, '菜单管理', 'Menu', 'system/menu/index', 5, 'menu', 'menu', b'0', b'0', b'0', 'menu:list', NULL,
        NULL, '2018-12-18 15:17:28', NULL);
INSERT INTO `sys_menu`
VALUES (6, NULL, 4, 0, '系统监控', NULL, NULL, 10, 'monitor', 'monitor', b'0', b'0', b'0', NULL, NULL, NULL,
        '2018-12-18 15:17:48', NULL);
INSERT INTO `sys_menu`
VALUES (7, 6, 0, 1, '操作日志', 'Log', 'monitor/log/index', 11, 'log', 'logs', b'0', b'1', b'0', NULL, NULL, 'admin',
        '2018-12-18 15:18:26', '2020-06-06 13:11:57');
INSERT INTO `sys_menu`
VALUES (9, 6, 0, 1, 'SQL监控', 'Sql', 'monitor/sql/index', 18, 'sqlMonitor', 'druid', b'0', b'0', b'0', NULL, NULL,
        NULL, '2018-12-18 15:19:34', NULL);
INSERT INTO `sys_menu`
VALUES (10, NULL, 5, 0, '组件管理', NULL, NULL, 50, 'zujian', 'components', b'0', b'0', b'0', NULL, NULL, NULL,
        '2018-12-19 13:38:16', NULL);
INSERT INTO `sys_menu`
VALUES (11, 10, 0, 1, '图标库', 'Icons', 'components/icons/index', 51, 'icon', 'icon', b'0', b'0', b'0', NULL, NULL,
        NULL, '2018-12-19 13:38:49', NULL);
INSERT INTO `sys_menu`
VALUES (14, 36, 0, 1, '邮件工具', 'Email', 'tools/email/index', 35, 'email', 'email', b'0', b'0', b'0', NULL, NULL,
        NULL, '2018-12-27 10:13:09', NULL);
INSERT INTO `sys_menu`
VALUES (15, 10, 0, 1, '富文本', 'Editor', 'components/Editor', 52, 'fwb', 'tinymce', b'0', b'0', b'0', NULL, NULL, NULL,
        '2018-12-27 11:58:25', NULL);
INSERT INTO `sys_menu`
VALUES (18, 36, 3, 1, '存储管理', 'Storage', 'tools/storage/index', 34, 'qiniu', 'storage', b'0', b'0', b'0',
        'storage:list', NULL, NULL, '2018-12-31 11:12:15', NULL);
INSERT INTO `sys_menu`
VALUES (21, NULL, 2, 0, '多级菜单', NULL, '', 900, 'menu', 'nested', b'0', b'0', b'0', NULL, NULL, 'admin',
        '2019-01-04 16:22:03', '2020-06-21 17:27:35');
INSERT INTO `sys_menu`
VALUES (22, 21, 2, 0, '二级菜单1', NULL, '', 999, 'menu', 'menu1', b'0', b'0', b'0', NULL, NULL, 'admin',
        '2019-01-04 16:23:29', '2020-06-21 17:27:20');
INSERT INTO `sys_menu`
VALUES (23, 21, 0, 1, '二级菜单2', NULL, 'nested/menu2/index', 999, 'menu', 'menu2', b'0', b'0', b'0', NULL, NULL, NULL,
        '2019-01-04 16:23:57', NULL);
INSERT INTO `sys_menu`
VALUES (24, 22, 0, 1, '三级菜单1', 'Test', 'nested/menu1/menu1-1', 999, 'menu', 'menu1-1', b'0', b'0', b'0', NULL, NULL,
        NULL, '2019-01-04 16:24:48', NULL);
INSERT INTO `sys_menu`
VALUES (27, 22, 0, 1, '三级菜单2', NULL, 'nested/menu1/menu1-2', 999, 'menu', 'menu1-2', b'0', b'0', b'0', NULL, NULL,
        NULL, '2019-01-07 17:27:32', NULL);
INSERT INTO `sys_menu`
VALUES (28, 1, 3, 1, '任务调度', 'Timing', 'system/timing/index', 999, 'timing', 'timing', b'0', b'0', b'0',
        'timing:list', NULL, NULL, '2019-01-07 20:34:40', NULL);
INSERT INTO `sys_menu`
VALUES (30, 36, 0, 1, '代码生成', 'GeneratorIndex', 'generator/index', 32, 'dev', 'generator', b'0', b'1', b'0', NULL,
        NULL, NULL, '2019-01-11 15:45:55', NULL);
INSERT INTO `sys_menu`
VALUES (32, 6, 0, 1, '异常日志', 'ErrorLog', 'monitor/log/errorLog', 12, 'error', 'errorLog', b'0', b'0', b'0', NULL,
        NULL, NULL, '2019-01-13 13:49:03', NULL);
INSERT INTO `sys_menu`
VALUES (33, 10, 0, 1, 'Markdown', 'Markdown', 'components/MarkDown', 53, 'markdown', 'markdown', b'0', b'0', b'0', NULL,
        NULL, NULL, '2019-03-08 13:46:44', NULL);
INSERT INTO `sys_menu`
VALUES (34, 10, 0, 1, 'Yaml编辑器', 'YamlEdit', 'components/YamlEdit', 54, 'dev', 'yaml', b'0', b'0', b'0', NULL, NULL,
        NULL, '2019-03-08 15:49:40', NULL);
INSERT INTO `sys_menu`
VALUES (35, 1, 3, 1, '部门管理', 'Dept', 'system/dept/index', 6, 'dept', 'dept', b'0', b'0', b'0', 'dept:list', NULL,
        NULL, '2019-03-25 09:46:00', NULL);
INSERT INTO `sys_menu`
VALUES (36, NULL, 6, 0, '系统工具', NULL, '', 30, 'sys-tools', 'sys-tools', b'0', b'0', b'0', NULL, NULL, NULL,
        '2019-03-29 10:57:35', NULL);
INSERT INTO `sys_menu`
VALUES (37, 1, 3, 1, '岗位管理', 'Job', 'system/job/index', 7, 'Steve-Jobs', 'job', b'0', b'0', b'0', 'job:list', NULL,
        NULL, '2019-03-29 13:51:18', NULL);
INSERT INTO `sys_menu`
VALUES (38, 36, 0, 1, '接口文档', 'Swagger', 'tools/swagger/index', 36, 'swagger', 'swagger2', b'0', b'0', b'0', NULL,
        NULL, NULL, '2019-03-29 19:57:53', NULL);
INSERT INTO `sys_menu`
VALUES (39, 1, 3, 1, '字典管理', 'Dict', 'system/dict/index', 8, 'dictionary', 'dict', b'0', b'0', b'0', 'dict:list',
        NULL, NULL, '2019-04-10 11:49:04', NULL);
INSERT INTO `sys_menu`
VALUES (44, 2, 0, 2, '用户新增', NULL, '', 2, '', '', b'0', b'0', b'0', 'user:add', NULL, NULL, '2019-10-29 10:59:46',
        NULL);
INSERT INTO `sys_menu`
VALUES (45, 2, 0, 2, '用户编辑', NULL, '', 3, '', '', b'0', b'0', b'0', 'user:edit', NULL, NULL, '2019-10-29 11:00:08',
        NULL);
INSERT INTO `sys_menu`
VALUES (46, 2, 0, 2, '用户删除', NULL, '', 4, '', '', b'0', b'0', b'0', 'user:del', NULL, NULL, '2019-10-29 11:00:23',
        NULL);
INSERT INTO `sys_menu`
VALUES (48, 3, 0, 2, '角色创建', NULL, '', 2, '', '', b'0', b'0', b'0', 'roles:add', NULL, NULL, '2019-10-29 12:45:34',
        NULL);
INSERT INTO `sys_menu`
VALUES (49, 3, 0, 2, '角色修改', NULL, '', 3, '', '', b'0', b'0', b'0', 'roles:edit', NULL, NULL, '2019-10-29 12:46:16',
        NULL);
INSERT INTO `sys_menu`
VALUES (50, 3, 0, 2, '角色删除', NULL, '', 4, '', '', b'0', b'0', b'0', 'roles:del', NULL, NULL, '2019-10-29 12:46:51',
        NULL);
INSERT INTO `sys_menu`
VALUES (52, 5, 0, 2, '菜单新增', NULL, '', 2, '', '', b'0', b'0', b'0', 'menu:add', NULL, NULL, '2019-10-29 12:55:07',
        NULL);
INSERT INTO `sys_menu`
VALUES (53, 5, 0, 2, '菜单编辑', NULL, '', 3, '', '', b'0', b'0', b'0', 'menu:edit', NULL, NULL, '2019-10-29 12:55:40',
        NULL);
INSERT INTO `sys_menu`
VALUES (54, 5, 0, 2, '菜单删除', NULL, '', 4, '', '', b'0', b'0', b'0', 'menu:del', NULL, NULL, '2019-10-29 12:56:00',
        NULL);
INSERT INTO `sys_menu`
VALUES (56, 35, 0, 2, '部门新增', NULL, '', 2, '', '', b'0', b'0', b'0', 'dept:add', NULL, NULL, '2019-10-29 12:57:09',
        NULL);
INSERT INTO `sys_menu`
VALUES (57, 35, 0, 2, '部门编辑', NULL, '', 3, '', '', b'0', b'0', b'0', 'dept:edit', NULL, NULL, '2019-10-29 12:57:27',
        NULL);
INSERT INTO `sys_menu`
VALUES (58, 35, 0, 2, '部门删除', NULL, '', 4, '', '', b'0', b'0', b'0', 'dept:del', NULL, NULL, '2019-10-29 12:57:41',
        NULL);
INSERT INTO `sys_menu`
VALUES (60, 37, 0, 2, '岗位新增', NULL, '', 2, '', '', b'0', b'0', b'0', 'job:add', NULL, NULL, '2019-10-29 12:58:27',
        NULL);
INSERT INTO `sys_menu`
VALUES (61, 37, 0, 2, '岗位编辑', NULL, '', 3, '', '', b'0', b'0', b'0', 'job:edit', NULL, NULL, '2019-10-29 12:58:45',
        NULL);
INSERT INTO `sys_menu`
VALUES (62, 37, 0, 2, '岗位删除', NULL, '', 4, '', '', b'0', b'0', b'0', 'job:del', NULL, NULL, '2019-10-29 12:59:04',
        NULL);
INSERT INTO `sys_menu`
VALUES (64, 39, 0, 2, '字典新增', NULL, '', 2, '', '', b'0', b'0', b'0', 'dict:add', NULL, NULL, '2019-10-29 13:00:17',
        NULL);
INSERT INTO `sys_menu`
VALUES (65, 39, 0, 2, '字典编辑', NULL, '', 3, '', '', b'0', b'0', b'0', 'dict:edit', NULL, NULL, '2019-10-29 13:00:42',
        NULL);
INSERT INTO `sys_menu`
VALUES (66, 39, 0, 2, '字典删除', NULL, '', 4, '', '', b'0', b'0', b'0', 'dict:del', NULL, NULL, '2019-10-29 13:00:59',
        NULL);
INSERT INTO `sys_menu`
VALUES (73, 28, 0, 2, '任务新增', NULL, '', 2, '', '', b'0', b'0', b'0', 'timing:add', NULL, NULL,
        '2019-10-29 13:07:28', NULL);
INSERT INTO `sys_menu`
VALUES (74, 28, 0, 2, '任务编辑', NULL, '', 3, '', '', b'0', b'0', b'0', 'timing:edit', NULL, NULL,
        '2019-10-29 13:07:41', NULL);
INSERT INTO `sys_menu`
VALUES (75, 28, 0, 2, '任务删除', NULL, '', 4, '', '', b'0', b'0', b'0', 'timing:del', NULL, NULL,
        '2019-10-29 13:07:54', NULL);
INSERT INTO `sys_menu`
VALUES (77, 18, 0, 2, '上传文件', NULL, '', 2, '', '', b'0', b'0', b'0', 'storage:add', NULL, NULL,
        '2019-10-29 13:09:09', NULL);
INSERT INTO `sys_menu`
VALUES (78, 18, 0, 2, '文件编辑', NULL, '', 3, '', '', b'0', b'0', b'0', 'storage:edit', NULL, NULL,
        '2019-10-29 13:09:22', NULL);
INSERT INTO `sys_menu`
VALUES (79, 18, 0, 2, '文件删除', NULL, '', 4, '', '', b'0', b'0', b'0', 'storage:del', NULL, NULL,
        '2019-10-29 13:09:34', NULL);
INSERT INTO `sys_menu`
VALUES (80, 6, 0, 1, '服务监控', 'ServerMonitor', 'monitor/server/index', 14, 'codeConsole', 'server', b'0', b'0', b'0',
        'monitor:list', NULL, 'admin', '2019-11-07 13:06:39', '2020-05-04 18:20:50');
INSERT INTO `sys_menu`
VALUES (82, 36, 0, 1, '生成配置', 'GeneratorConfig', 'generator/config', 33, 'dev', 'generator/config/:tableName', b'0',
        b'1', b'1', '', NULL, NULL, '2019-11-17 20:08:56', NULL);
INSERT INTO `sys_menu`
VALUES (83, 10, 0, 1, '图表库', 'Echarts', 'components/Echarts', 50, 'chart', 'echarts', b'0', b'1', b'0', '', NULL,
        NULL, '2019-11-21 09:04:32', NULL);
INSERT INTO `sys_menu`
VALUES (116, 36, 0, 1, '生成预览', 'Preview', 'generator/preview', 999, 'java', 'generator/preview/:tableName', b'0',
        b'1', b'1', NULL, NULL, NULL, '2019-11-26 14:54:36', NULL);
INSERT INTO `sys_menu`
VALUES (30116, 1, 0, 1, '资源管理', 'Resource', 'system/resource/index', 5, 'backup', 'resource', b'0', b'0', b'0',
        NULL, 'admin', 'admin', '2023-06-16 20:25:50', '2023-06-16 20:31:48');
COMMIT;

-- ----------------------------
-- Table structure for sys_quartz_job
-- ----------------------------
DROP TABLE IF EXISTS `sys_quartz_job`;
CREATE TABLE `sys_quartz_job`
(
    `job_id`              bigint NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `bean_name`           varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT 'Spring Bean名称',
    `cron_expression`     varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT 'cron 表达式',
    `is_pause`            bit(1)                                                  DEFAULT NULL COMMENT '状态：1暂停、0启用',
    `job_name`            varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '任务名称',
    `method_name`         varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '方法名称',
    `params`              varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '参数',
    `description`         varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '备注',
    `person_in_charge`    varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '负责人',
    `email`               varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '报警邮箱',
    `sub_task`            varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '子任务ID',
    `pause_after_failure` bit(1)                                                  DEFAULT NULL COMMENT '任务失败后是否暂停',
    `create_by`           varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建者',
    `update_by`           varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '更新者',
    `create_time`         datetime                                                DEFAULT NULL COMMENT '创建日期',
    `update_time`         datetime                                                DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`job_id`) USING BTREE,
    KEY `inx_is_pause` (`is_pause`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 7
  DEFAULT CHARSET = utf8mb3 COMMENT ='定时任务';

-- ----------------------------
-- Records of sys_quartz_job
-- ----------------------------
BEGIN;
INSERT INTO `sys_quartz_job`
VALUES (2, 'testTask', '0/5 * * * * ?', b'1', '测试1', 'run1', 'test', '带参测试，多参使用json', '测试', NULL, NULL,
        NULL, NULL, 'admin', '2019-08-22 14:08:29', '2020-05-24 13:58:33');
INSERT INTO `sys_quartz_job`
VALUES (3, 'testTask', '0/5 * * * * ?', b'1', '测试', 'run', '', '不带参测试', 'Zheng Jie', '', '5,6', b'1', NULL,
        'admin', '2019-09-26 16:44:39', '2020-05-24 14:48:12');
INSERT INTO `sys_quartz_job`
VALUES (6, 'testTask', '0/5 * * * * ?', b'1', '测试3', 'run2', NULL, '测试3', 'Zheng Jie', '', NULL, b'1', 'admin',
        'admin', '2020-05-05 20:35:41', '2020-05-05 20:36:07');
COMMIT;

-- ----------------------------
-- Table structure for sys_quartz_log
-- ----------------------------
DROP TABLE IF EXISTS `sys_quartz_log`;
CREATE TABLE `sys_quartz_log`
(
    `log_id`           bigint NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `bean_name`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `create_time`      datetime                                                DEFAULT NULL,
    `cron_expression`  varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `exception_detail` text CHARACTER SET utf8 COLLATE utf8_general_ci,
    `is_success`       bit(1)                                                  DEFAULT NULL,
    `job_name`         varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `method_name`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `params`           varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL,
    `time`             bigint                                                  DEFAULT NULL,
    PRIMARY KEY (`log_id`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 152
  DEFAULT CHARSET = utf8mb3 COMMENT ='定时任务日志';

-- ----------------------------
-- Records of sys_quartz_log
-- ----------------------------
BEGIN;
INSERT INTO `sys_quartz_log`
VALUES (151, 'Test', '2021-11-23 15:56:47', '0/5 * * * * ?',
        'org.springframework.beans.factory.NoSuchBeanDefinitionException: No bean named \'Test\' available\n	at org.springframework.beans.factory.support.DefaultListableBeanFactory.getBeanDefinition(DefaultListableBeanFactory.java:863)\n	at org.springframework.beans.factory.support.AbstractBeanFactory.getMergedLocalBeanDefinition(AbstractBeanFactory.java:1344)\n	at org.springframework.beans.factory.support.AbstractBeanFactory.doGetBean(AbstractBeanFactory.java:309)\n	at org.springframework.beans.factory.support.AbstractBeanFactory.getBean(AbstractBeanFactory.java:208)\n	at org.springframework.context.support.AbstractApplicationContext.getBean(AbstractApplicationContext.java:1154)\n	at com.lwohvye.utils.SpringContextHolder.getBean(SpringContextHolder.java:93)\n	at com.lwohvye.modules.quartz.utils.QuartzRunnable.<init>(QuartzRunnable.java:38)\n	at com.lwohvye.modules.quartz.utils.ExecutionJob.executeInternal(ExecutionJob.java:81)\n	at org.springframework.scheduling.quartz.QuartzJobBean.execute(QuartzJobBean.java:75)\n	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)\n	at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:77)\n	at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)\n	at java.base/java.lang.reflect.Method.invoke(Method.java:568)\n	at org.springframework.aop.support.AopUtils.invokeJoinpointUsingReflection(AopUtils.java:344)\n	at org.springframework.aop.framework.ReflectiveMethodInvocation.invokeJoinpoint(ReflectiveMethodInvocation.java:198)\n	at org.springframework.aop.framework.ReflectiveMethodInvocation.proceed(ReflectiveMethodInvocation.java:163)\n	at org.springframework.aop.interceptor.AsyncExecutionInterceptor.lambda$invoke$0(AsyncExecutionInterceptor.java:115)\n	at java.base/java.util.concurrent.FutureTask.run(FutureTask.java:264)\n	at java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1136)\n	at java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:635)\n	at java.base/java.lang.Thread.run(Thread.java:833)\n',
        b'0', '任务告警测试', 'run', NULL, 5);
COMMIT;

-- ----------------------------
-- Table structure for sys_resource
-- ----------------------------
DROP TABLE IF EXISTS `sys_resource`;
CREATE TABLE `sys_resource`
(
    `resource_id` bigint                                                        NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `name`        varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '资源名称',
    `pattern`     varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT 'URI',
    `req_method`  varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '请求方法 空表示全部',
    `status`      tinyint                                                       DEFAULT '1' COMMENT '状态 0-不可用 1-可用',
    `rest_name`   varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '所在类名',
    `remark`      varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '备注',
    PRIMARY KEY (`resource_id`),
    KEY `pattern` (`pattern`)
) ENGINE = InnoDB
  AUTO_INCREMENT = 30021
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT ='资源（API）';

-- ----------------------------
-- Records of sys_resource
-- ----------------------------
BEGIN;
INSERT INTO `sys_resource`
VALUES (1, '查询', '/api/**', 'GET', 1, '', '所有查询权限');
INSERT INTO `sys_resource`
VALUES (2, '修改', '/api/**', 'PUT', 1, '', '所有修改权限');
INSERT INTO `sys_resource`
VALUES (3, '新增', '/api/**', 'POST', 1, '', '所有新增权限');
INSERT INTO `sys_resource`
VALUES (4, '删除', '/api/**', 'DELETE', 1, '', '所有删除权限');
INSERT INTO `sys_resource`
VALUES (5, '角色模块', '/api/sys/roles**', NULL, 1, 'RoleRest', '角色模块所有权限');
INSERT INTO `sys_resource`
VALUES (6, '资源查询', '/api/sys/resources*', 'GET', 1, 'ResourceRest', '资源查询权限');
INSERT INTO `sys_resource`
VALUES (7, '系统管理', '/api/sys/**', NULL, 1, NULL, '系统管理模块权限');
INSERT INTO `sys_resource`
VALUES (8, '验证码', '/captcha/**', NULL, 1, NULL, '获取验证码');
INSERT INTO `sys_resource`
VALUES (9, 'html资源', '/**/*.html', 'GET', 1, NULL, NULL);
INSERT INTO `sys_resource`
VALUES (10, 'css资源', '/**/*.css', 'GET', 1, NULL, NULL);
INSERT INTO `sys_resource`
VALUES (11, 'js资源', '/**/*.js', 'GET', 1, NULL, NULL);
INSERT INTO `sys_resource`
VALUES (12, 'api-docs', '/v3/api-docs/**', NULL, 1, NULL, NULL);
INSERT INTO `sys_resource`
VALUES (13, 'swagger-ui', '/swagger-ui/**', NULL, 1, NULL, NULL);
INSERT INTO `sys_resource`
VALUES (14, 'avatar', '/avatar/**', NULL, 1, NULL, NULL);
INSERT INTO `sys_resource`
VALUES (15, 'file', '/file/**', NULL, 1, NULL, NULL);
INSERT INTO `sys_resource`
VALUES (16, 'druid', '/druid/**', NULL, 1, NULL, NULL);
INSERT INTO `sys_resource`
VALUES (17, 'options', '/**', 'OPTIONS', 1, NULL, NULL);
INSERT INTO `sys_resource`
VALUES (18, 'webSocket', '/webSocket/**', NULL, 1, NULL, 'WebSocket相关');
INSERT INTO `sys_resource`
VALUES (19, '匿名标记', '/api/anonymous/**', NULL, 1, NULL, '通一匿名标记');
INSERT INTO `sys_resource`
VALUES (20, 'errorPage', '/error', '', 1, NULL, '错误页');
INSERT INTO `sys_resource`
VALUES (30020, 'ico图标资源', '/**/*.ico', NULL, 1, NULL, '获取图标资源，unused');
COMMIT;

-- ----------------------------
-- Table structure for sys_role
-- ----------------------------
DROP TABLE IF EXISTS `sys_role`;
CREATE TABLE `sys_role`
(
    `role_id`     bigint                                                  NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `name`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '名称',
    `code`        varchar(255)                                            DEFAULT NULL COMMENT '权限标识',
    `level`       int                                                     DEFAULT NULL COMMENT '角色级别',
    `description` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '描述',
    `data_scope`  varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '数据权限',
    `create_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建者',
    `update_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '更新者',
    `create_time` datetime                                                DEFAULT NULL COMMENT '创建日期',
    `update_time` datetime                                                DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`role_id`) USING BTREE,
    UNIQUE KEY `uniq_name` (`name`) USING BTREE,
    KEY `role_name_index` (`name`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 17
  DEFAULT CHARSET = utf8mb3 COMMENT ='角色表';

-- ----------------------------
-- Records of sys_role
-- ----------------------------
BEGIN;
INSERT INTO `sys_role`
VALUES (1, '超级管理员', 'admin', 1, '- 看起来比较菜', '全部', NULL, 'admin', '2018-11-23 11:04:37',
        '2023-06-16 20:27:40');
INSERT INTO `sys_role`
VALUES (2, '普通用户', 'commin', 2, '普普通通，平平坦坦', '本级', NULL, 'admin', '2018-11-23 13:09:06',
        '2022-12-13 09:46:06');
INSERT INTO `sys_role`
VALUES (3, '产品运营', 'product', 2, '产品运营及输出，瑞star', '本级', 'admin', 'admin', '2021-11-05 22:40:03',
        '2022-10-09 20:02:21');
INSERT INTO `sys_role`
VALUES (4, '大佬', 'dalao', 4, NULL, '自定义', 'admin', 'admin', '2021-11-06 19:57:46', '2021-11-09 21:03:27');
INSERT INTO `sys_role`
VALUES (5, 'Java 从精通到陌生', 'java', 3, NULL, '本级', 'admin', 'admin', '2021-11-08 12:25:28',
        '2022-12-13 09:12:02');
INSERT INTO `sys_role`
VALUES (6, 'C语言 从看懂到看开', 'c', 3, NULL, '本级', 'admin', 'admin', '2021-11-08 12:25:58', '2022-12-13 09:40:02');
INSERT INTO `sys_role`
VALUES (7, 'Python', 'python', 3, NULL, '本级', 'admin', 'admin', '2021-11-08 12:26:08', '2021-11-08 12:26:08');
INSERT INTO `sys_role`
VALUES (8, 'IOS', 'ios', 3, NULL, '本级', 'admin', 'admin', '2021-11-08 12:26:31', '2021-11-08 12:26:31');
INSERT INTO `sys_role`
VALUES (9, 'Android 从入门到改行', 'android', 3, NULL, '本级', 'admin', 'admin', '2021-11-08 12:26:49',
        '2021-11-24 18:50:08');
INSERT INTO `sys_role`
VALUES (10, 'C++', 'c++', 3, NULL, '本级', 'admin', 'admin', '2021-11-08 12:27:02', '2021-11-08 12:27:02');
INSERT INTO `sys_role`
VALUES (11, 'JavaScript全栈开发 从入门到单身狗', 'js', 3, NULL, '本级', 'admin', 'admin', '2021-11-08 12:27:28',
        '2021-11-24 18:53:11');
INSERT INTO `sys_role`
VALUES (12, 'Golang', 'go', 3, NULL, '本级', 'admin', 'admin', '2021-11-08 12:28:04', '2022-03-13 21:59:41');
INSERT INTO `sys_role`
VALUES (13, 'PHP 从放弃到坚持放弃', 'php', 3, NULL, '本级', 'admin', 'admin', '2021-11-08 12:28:13',
        '2021-11-24 18:51:15');
INSERT INTO `sys_role`
VALUES (14, 'C# 从入门到放弃', 'c#', 3, NULL, '全部', 'admin', 'admin', '2021-11-24 18:50:45', '2021-11-24 18:50:45');
INSERT INTO `sys_role`
VALUES (15, 'SQL Server 没入门就放弃', 'sql_server', 3, NULL, '全部', 'admin', 'admin', '2021-11-24 18:53:44',
        '2021-11-24 18:53:44');
INSERT INTO `sys_role`
VALUES (16, '匿名', 'anonymous', 4, '五彩斑斓的透明', '本级', 'admin', 'admin', '2021-11-30 10:10:20',
        '2022-03-20 00:19:38');
COMMIT;

-- ----------------------------
-- Table structure for sys_roles_depts
-- ----------------------------
DROP TABLE IF EXISTS `sys_roles_depts`;
CREATE TABLE `sys_roles_depts`
(
    `role_id` bigint NOT NULL,
    `dept_id` bigint NOT NULL,
    PRIMARY KEY (`role_id`, `dept_id`) USING BTREE,
    KEY `FK7qg6itn5ajdoa9h9o78v9ksur` (`dept_id`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb3 COMMENT ='角色部门关联';

-- ----------------------------
-- Records of sys_roles_depts
-- ----------------------------
BEGIN;
INSERT INTO `sys_roles_depts`
VALUES (4, 2);
INSERT INTO `sys_roles_depts`
VALUES (4, 6);
COMMIT;

-- ----------------------------
-- Table structure for sys_roles_menus
-- ----------------------------
DROP TABLE IF EXISTS `sys_roles_menus`;
CREATE TABLE `sys_roles_menus`
(
    `menu_id` bigint NOT NULL COMMENT '菜单ID',
    `role_id` bigint NOT NULL COMMENT '角色ID',
    PRIMARY KEY (`menu_id`, `role_id`) USING BTREE,
    KEY `FKcngg2qadojhi3a651a5adkvbq` (`role_id`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb3 COMMENT ='角色菜单关联';

-- ----------------------------
-- Records of sys_roles_menus
-- ----------------------------
BEGIN;
INSERT INTO `sys_roles_menus`
VALUES (1, 1);
INSERT INTO `sys_roles_menus`
VALUES (2, 1);
INSERT INTO `sys_roles_menus`
VALUES (3, 1);
INSERT INTO `sys_roles_menus`
VALUES (5, 1);
INSERT INTO `sys_roles_menus`
VALUES (6, 1);
INSERT INTO `sys_roles_menus`
VALUES (7, 1);
INSERT INTO `sys_roles_menus`
VALUES (9, 1);
INSERT INTO `sys_roles_menus`
VALUES (10, 1);
INSERT INTO `sys_roles_menus`
VALUES (11, 1);
INSERT INTO `sys_roles_menus`
VALUES (14, 1);
INSERT INTO `sys_roles_menus`
VALUES (15, 1);
INSERT INTO `sys_roles_menus`
VALUES (18, 1);
INSERT INTO `sys_roles_menus`
VALUES (21, 1);
INSERT INTO `sys_roles_menus`
VALUES (22, 1);
INSERT INTO `sys_roles_menus`
VALUES (23, 1);
INSERT INTO `sys_roles_menus`
VALUES (24, 1);
INSERT INTO `sys_roles_menus`
VALUES (27, 1);
INSERT INTO `sys_roles_menus`
VALUES (28, 1);
INSERT INTO `sys_roles_menus`
VALUES (30, 1);
INSERT INTO `sys_roles_menus`
VALUES (32, 1);
INSERT INTO `sys_roles_menus`
VALUES (33, 1);
INSERT INTO `sys_roles_menus`
VALUES (34, 1);
INSERT INTO `sys_roles_menus`
VALUES (35, 1);
INSERT INTO `sys_roles_menus`
VALUES (36, 1);
INSERT INTO `sys_roles_menus`
VALUES (37, 1);
INSERT INTO `sys_roles_menus`
VALUES (38, 1);
INSERT INTO `sys_roles_menus`
VALUES (39, 1);
INSERT INTO `sys_roles_menus`
VALUES (44, 1);
INSERT INTO `sys_roles_menus`
VALUES (45, 1);
INSERT INTO `sys_roles_menus`
VALUES (46, 1);
INSERT INTO `sys_roles_menus`
VALUES (48, 1);
INSERT INTO `sys_roles_menus`
VALUES (49, 1);
INSERT INTO `sys_roles_menus`
VALUES (50, 1);
INSERT INTO `sys_roles_menus`
VALUES (52, 1);
INSERT INTO `sys_roles_menus`
VALUES (53, 1);
INSERT INTO `sys_roles_menus`
VALUES (54, 1);
INSERT INTO `sys_roles_menus`
VALUES (56, 1);
INSERT INTO `sys_roles_menus`
VALUES (57, 1);
INSERT INTO `sys_roles_menus`
VALUES (58, 1);
INSERT INTO `sys_roles_menus`
VALUES (60, 1);
INSERT INTO `sys_roles_menus`
VALUES (61, 1);
INSERT INTO `sys_roles_menus`
VALUES (62, 1);
INSERT INTO `sys_roles_menus`
VALUES (64, 1);
INSERT INTO `sys_roles_menus`
VALUES (65, 1);
INSERT INTO `sys_roles_menus`
VALUES (66, 1);
INSERT INTO `sys_roles_menus`
VALUES (73, 1);
INSERT INTO `sys_roles_menus`
VALUES (74, 1);
INSERT INTO `sys_roles_menus`
VALUES (75, 1);
INSERT INTO `sys_roles_menus`
VALUES (77, 1);
INSERT INTO `sys_roles_menus`
VALUES (78, 1);
INSERT INTO `sys_roles_menus`
VALUES (79, 1);
INSERT INTO `sys_roles_menus`
VALUES (80, 1);
INSERT INTO `sys_roles_menus`
VALUES (82, 1);
INSERT INTO `sys_roles_menus`
VALUES (83, 1);
INSERT INTO `sys_roles_menus`
VALUES (116, 1);
INSERT INTO `sys_roles_menus`
VALUES (30116, 1);
INSERT INTO `sys_roles_menus`
VALUES (1, 2);
INSERT INTO `sys_roles_menus`
VALUES (2, 2);
INSERT INTO `sys_roles_menus`
VALUES (3, 2);
INSERT INTO `sys_roles_menus`
VALUES (6, 2);
INSERT INTO `sys_roles_menus`
VALUES (7, 2);
INSERT INTO `sys_roles_menus`
VALUES (9, 2);
INSERT INTO `sys_roles_menus`
VALUES (32, 2);
INSERT INTO `sys_roles_menus`
VALUES (35, 2);
INSERT INTO `sys_roles_menus`
VALUES (37, 2);
INSERT INTO `sys_roles_menus`
VALUES (39, 2);
INSERT INTO `sys_roles_menus`
VALUES (44, 2);
INSERT INTO `sys_roles_menus`
VALUES (45, 2);
INSERT INTO `sys_roles_menus`
VALUES (49, 2);
INSERT INTO `sys_roles_menus`
VALUES (57, 2);
INSERT INTO `sys_roles_menus`
VALUES (60, 2);
INSERT INTO `sys_roles_menus`
VALUES (64, 2);
INSERT INTO `sys_roles_menus`
VALUES (65, 2);
INSERT INTO `sys_roles_menus`
VALUES (66, 2);
INSERT INTO `sys_roles_menus`
VALUES (80, 2);
INSERT INTO `sys_roles_menus`
VALUES (1, 3);
INSERT INTO `sys_roles_menus`
VALUES (10, 3);
INSERT INTO `sys_roles_menus`
VALUES (14, 3);
INSERT INTO `sys_roles_menus`
VALUES (15, 3);
INSERT INTO `sys_roles_menus`
VALUES (18, 3);
INSERT INTO `sys_roles_menus`
VALUES (21, 3);
INSERT INTO `sys_roles_menus`
VALUES (22, 3);
INSERT INTO `sys_roles_menus`
VALUES (23, 3);
INSERT INTO `sys_roles_menus`
VALUES (24, 3);
INSERT INTO `sys_roles_menus`
VALUES (27, 3);
INSERT INTO `sys_roles_menus`
VALUES (33, 3);
INSERT INTO `sys_roles_menus`
VALUES (34, 3);
INSERT INTO `sys_roles_menus`
VALUES (36, 3);
INSERT INTO `sys_roles_menus`
VALUES (39, 3);
INSERT INTO `sys_roles_menus`
VALUES (64, 3);
INSERT INTO `sys_roles_menus`
VALUES (65, 3);
INSERT INTO `sys_roles_menus`
VALUES (66, 3);
INSERT INTO `sys_roles_menus`
VALUES (77, 3);
INSERT INTO `sys_roles_menus`
VALUES (78, 3);
INSERT INTO `sys_roles_menus`
VALUES (14, 4);
INSERT INTO `sys_roles_menus`
VALUES (2, 5);
INSERT INTO `sys_roles_menus`
VALUES (44, 5);
INSERT INTO `sys_roles_menus`
VALUES (45, 5);
INSERT INTO `sys_roles_menus`
VALUES (7, 6);
INSERT INTO `sys_roles_menus`
VALUES (9, 6);
INSERT INTO `sys_roles_menus`
VALUES (14, 6);
INSERT INTO `sys_roles_menus`
VALUES (44, 6);
INSERT INTO `sys_roles_menus`
VALUES (45, 6);
INSERT INTO `sys_roles_menus`
VALUES (48, 6);
INSERT INTO `sys_roles_menus`
VALUES (49, 6);
INSERT INTO `sys_roles_menus`
VALUES (52, 6);
INSERT INTO `sys_roles_menus`
VALUES (53, 6);
INSERT INTO `sys_roles_menus`
VALUES (56, 6);
INSERT INTO `sys_roles_menus`
VALUES (57, 6);
INSERT INTO `sys_roles_menus`
VALUES (60, 6);
INSERT INTO `sys_roles_menus`
VALUES (61, 6);
COMMIT;

-- ----------------------------
-- Table structure for sys_roles_resources
-- ----------------------------
DROP TABLE IF EXISTS `sys_roles_resources`;
CREATE TABLE `sys_roles_resources`
(
    `role_id`     bigint NOT NULL,
    `resource_id` bigint NOT NULL,
    PRIMARY KEY (`role_id`, `resource_id`),
    KEY `role_id` (`role_id`),
    KEY `resource_id` (`resource_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_unicode_ci;

-- ----------------------------
-- Records of sys_roles_resources
-- ----------------------------
BEGIN;
INSERT INTO `sys_roles_resources`
VALUES (16, 8);
INSERT INTO `sys_roles_resources`
VALUES (16, 9);
INSERT INTO `sys_roles_resources`
VALUES (16, 10);
INSERT INTO `sys_roles_resources`
VALUES (16, 11);
INSERT INTO `sys_roles_resources`
VALUES (16, 12);
INSERT INTO `sys_roles_resources`
VALUES (16, 13);
INSERT INTO `sys_roles_resources`
VALUES (16, 14);
INSERT INTO `sys_roles_resources`
VALUES (16, 15);
INSERT INTO `sys_roles_resources`
VALUES (16, 16);
INSERT INTO `sys_roles_resources`
VALUES (16, 17);
INSERT INTO `sys_roles_resources`
VALUES (16, 18);
INSERT INTO `sys_roles_resources`
VALUES (16, 19);
INSERT INTO `sys_roles_resources`
VALUES (16, 20);
INSERT INTO `sys_roles_resources`
VALUES (16, 30020);
COMMIT;

-- ----------------------------
-- Table structure for sys_user
-- ----------------------------
DROP TABLE IF EXISTS `sys_user`;
CREATE TABLE `sys_user`
(
    `user_id`        bigint NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `dept_id`        bigint                                                  DEFAULT NULL COMMENT '部门名称',
    `username`       varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '用户名',
    `nick_name`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '昵称',
    `gender`         varchar(2) CHARACTER SET utf8 COLLATE utf8_general_ci   DEFAULT NULL COMMENT '性别',
    `phone`          varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '手机号码',
    `email`          varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '邮箱',
    `avatar_name`    varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '头像地址',
    `avatar_path`    varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '头像真实路径',
    `password`       varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '密码',
    `is_admin`       bit(1)                                                  DEFAULT b'0' COMMENT '是否为admin账号',
    `enabled`        bigint                                                  DEFAULT NULL COMMENT '状态：1启用、0禁用',
    `description`    mediumblob COMMENT '描述信息',
    `create_by`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建者',
    `update_by`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '更新者',
    `pwd_reset_time` datetime                                                DEFAULT NULL COMMENT '修改密码的时间',
    `create_time`    datetime                                                DEFAULT NULL COMMENT '创建日期',
    `update_time`    datetime                                                DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`user_id`) USING BTREE,
    UNIQUE KEY `UK_kpubos9gc2cvtkb0thktkbkes` (`email`) USING BTREE,
    UNIQUE KEY `username` (`username`) USING BTREE,
    UNIQUE KEY `uniq_username` (`username`) USING BTREE,
    UNIQUE KEY `uniq_email` (`email`) USING BTREE,
    KEY `FK5rwmryny6jthaaxkogownknqp` (`dept_id`) USING BTREE,
    KEY `FKpq2dhypk2qgt68nauh2by22jb` (`avatar_name`) USING BTREE,
    KEY `inx_enabled` (`enabled`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 4
  DEFAULT CHARSET = utf8mb3 COMMENT ='系统用户';

-- ----------------------------
-- Records of sys_user
-- ----------------------------
BEGIN;
INSERT INTO `sys_user`
VALUES (1, 2, 'admin', '管理员-咸鱼', '男', '18888888888', '201507802@qq.com', 'avatar-20200806032259161.png',
        '/Users/jie/Documents/work/me/admin/unicorn/~/avatar/avatar-20200806032259161.png',
        '$2a$10$tm56TdWtcFwRdo2VurgDUuqdThBEK0yN5kJBl9FHprXAnqz23hlL6', b'1', 1, 0xE592B8E9B1BC2DE4B88DE7BFBBE8BAAB,
        NULL, 'admin', '2021-11-11 12:25:14', '2018-08-23 09:11:56', '2021-11-24 18:45:19');
INSERT INTO `sys_user`
VALUES (2, 6, 'test', '测试-大佬', '男', '19999999999', 'test@unicorn.com', NULL, NULL,
        '$2a$10$4XcyudOYTSz6fue6KFNMHeUQnCX5jbBQypLEnGk1PmekXt5c95JcK', b'0', 1, 0xE4B880E58FAA206C6974746C6520,
        'admin', 'admin', '2022-04-03 11:48:23', '2020-05-05 11:15:49', '2022-04-01 07:54:28');
INSERT INTO `sys_user`
VALUES (3, 20, 'performer', '运营-产品', '女', '18288888888', 'perform@unicorn.com', NULL, NULL,
        '$2a$10$uUHeP2O2SJGAAcK6ZiLLDuaI/i/8.g4VpyW3EXI0n3FTJ9zsfADs2', b'0', 0, 0xE4BAA7E59381E8BF90E890A5, 'admin',
        'admin', '2022-04-01 11:53:12', '2021-11-05 22:38:56', '2023-07-15 22:43:34');
COMMIT;

-- ----------------------------
-- Table structure for sys_users_jobs
-- ----------------------------
DROP TABLE IF EXISTS `sys_users_jobs`;
CREATE TABLE `sys_users_jobs`
(
    `user_id` bigint NOT NULL COMMENT '用户ID',
    `job_id`  bigint NOT NULL COMMENT '岗位ID',
    PRIMARY KEY (`user_id`, `job_id`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb3;

-- ----------------------------
-- Records of sys_users_jobs
-- ----------------------------
BEGIN;
INSERT INTO `sys_users_jobs`
VALUES (1, 11);
INSERT INTO `sys_users_jobs`
VALUES (2, 12);
INSERT INTO `sys_users_jobs`
VALUES (3, 10);
INSERT INTO `sys_users_jobs`
VALUES (3, 15);
COMMIT;

-- ----------------------------
-- Table structure for sys_users_roles
-- ----------------------------
DROP TABLE IF EXISTS `sys_users_roles`;
CREATE TABLE `sys_users_roles`
(
    `user_id` bigint NOT NULL COMMENT '用户ID',
    `role_id` bigint NOT NULL COMMENT '角色ID',
    `id`      int    NOT NULL AUTO_INCREMENT,
    PRIMARY KEY (`id`, `user_id`, `role_id`) USING BTREE,
    KEY `FKq4eq273l04bpu4efj0jd0jb98` (`role_id`) USING BTREE,
    KEY `user_id` (`user_id`)
) ENGINE = InnoDB
  AUTO_INCREMENT = 8
  DEFAULT CHARSET = utf8mb3 COMMENT ='用户角色关联';

-- ----------------------------
-- Records of sys_users_roles
-- ----------------------------
BEGIN;
INSERT INTO `sys_users_roles`
VALUES (1, 1, 1);
INSERT INTO `sys_users_roles`
VALUES (2, 2, 2);
INSERT INTO `sys_users_roles`
VALUES (3, 3, 8);
INSERT INTO `sys_users_roles`
VALUES (2, 4, 4);
INSERT INTO `sys_users_roles`
VALUES (2, 6, 5);
COMMIT;

-- ----------------------------
-- Table structure for tool_email_config
-- ----------------------------
DROP TABLE IF EXISTS `tool_email_config`;
CREATE TABLE `tool_email_config`
(
    `config_id` bigint NOT NULL COMMENT 'ID',
    `from_user` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '收件人',
    `host`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '邮件服务器SMTP地址',
    `pass`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '密码',
    `port`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '端口',
    `user`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '发件者用户名',
    PRIMARY KEY (`config_id`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb3 COMMENT ='邮箱配置';

-- ----------------------------
-- Records of tool_email_config
-- ----------------------------
BEGIN;
COMMIT;

-- ----------------------------
-- Table structure for tool_local_storage
-- ----------------------------
DROP TABLE IF EXISTS `tool_local_storage`;
CREATE TABLE `tool_local_storage`
(
    `storage_id`  bigint NOT NULL AUTO_INCREMENT COMMENT 'ID',
    `real_name`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '文件真实的名称',
    `name`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '文件名',
    `suffix`      varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '后缀',
    `path`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '路径',
    `type`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '类型',
    `size`        varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '大小',
    `create_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建者',
    `update_by`   varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '更新者',
    `create_time` datetime                                                DEFAULT NULL COMMENT '创建日期',
    `update_time` datetime                                                DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`storage_id`) USING BTREE
) ENGINE = InnoDB
  AUTO_INCREMENT = 10
  DEFAULT CHARSET = utf8mb3 COMMENT ='本地存储';

-- ----------------------------
-- Records of tool_local_storage
-- ----------------------------
BEGIN;
COMMIT;

-- ----------------------------
-- View structure for sys_menu_view
-- ----------------------------
DROP VIEW IF EXISTS `sys_menu_view`;
CREATE ALGORITHM = UNDEFINED SQL SECURITY DEFINER VIEW `sys_menu_view` AS
select `sys_menu`.`menu_id`        AS `menu_id`,
       `sys_menu`.`title`          AS `title`,
       `sys_menu`.`name`           AS `name`,
       `sys_menu`.`type`           AS `type`,
       `sys_menu`.`pid`            AS `pid`,
       `sys_roles_menus`.`role_id` AS `role_id`
from (`sys_menu` join `sys_roles_menus`)
where (`sys_menu`.`menu_id` = `sys_roles_menus`.`menu_id`)
WITH CASCADED CHECK OPTION;

/*
 *    Copyright (c) 2024.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

-- ----------------------------
-- View structure for sys_role_view
-- ----------------------------
DROP VIEW IF EXISTS `sys_role_view`;
CREATE ALGORITHM = UNDEFINED SQL SECURITY DEFINER VIEW `sys_role_view` AS
select `sys_role`.`role_id`     AS `role_id`,
       `sys_role`.`name`        AS `name`,
       `sys_role`.`code`        AS `code`,
       `sys_role`.`description` AS `description`
from `sys_role`
WITH CASCADED CHECK OPTION;

-- ----------------------------
-- View structure for sys_user_view
-- ----------------------------
DROP VIEW IF EXISTS `sys_user_view`;
CREATE ALGORITHM = UNDEFINED SQL SECURITY DEFINER VIEW `sys_user_view` AS
select `sys_user`.`user_id`  AS `user_id`,
       `sys_user`.`username` AS `username`,
       `sys_user`.`phone`    AS `phone`,
       `sys_user`.`password` AS `password`,
       `sys_user`.`enabled`  AS `enabled`
from `sys_user`
WITH CASCADED CHECK OPTION;

SET FOREIGN_KEY_CHECKS = 1;
