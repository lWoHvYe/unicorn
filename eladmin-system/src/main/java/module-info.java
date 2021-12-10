module lwohvye.eladmin.system {
    requires transitive lwohvye.eladmin.api;
    requires transitive lwohvye.eladmin.logging;
    requires lwohvye.eladmin.tools;

    requires transitive spring.aop;
    requires transitive spring.amqp;
    requires transitive spring.context.support;
    requires transitive spring.expression;
    requires transitive spring.websocket;
    requires transitive spring.security.config;
    requires transitive spring.security.crypto;
    requires transitive spring.security.web;
    requires transitive spring.rabbit;
    requires transitive spring.retry;
    requires transitive jsch;
    requires transitive druid;
    requires transitive ganymed.ssh2;
    requires transitive org.apache.commons.io;
    requires transitive jakarta.websocket.api;
    requires transitive quartz;
    requires transitive easy.captcha;
    requires transitive redisson;
    requires transitive java.annotation;
    requires transitive java.management;
    requires transitive jjwt.api;
    requires transitive jjwt.impl;
    requires transitive jsr305;
    requires transitive com.github.oshi;
    requires transitive io.netty.common;

    exports com.lwohvye.modules.mnt.util;
    exports com.lwohvye.modules.rabbitmq.domain;
    exports com.lwohvye.modules.rabbitmq.service;
}
