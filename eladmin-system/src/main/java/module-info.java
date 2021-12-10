module lwohvye.eladmin.system {
    requires transitive lwohvye.eladmin.api;
    requires transitive lwohvye.eladmin.logging;
    requires lwohvye.eladmin.tools;

    requires spring.aop;
    requires spring.amqp;
    requires spring.context.support;
    requires spring.expression;
    requires spring.websocket;
    requires spring.security.config;
    requires spring.security.crypto;
    requires spring.security.web;
    requires spring.rabbit;
    requires spring.retry;
    requires jsch;
    requires druid;
    requires ganymed.ssh2;
    requires org.apache.commons.io;
    requires jakarta.websocket.api;
    requires quartz;
    requires easy.captcha;
    requires redisson;
    requires java.annotation;
    requires java.management;
    requires jjwt.api;
    requires jjwt.impl;
    requires jsr305;
    requires com.github.oshi;
    requires io.netty.common;
}
