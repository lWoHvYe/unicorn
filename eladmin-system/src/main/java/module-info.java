@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
module lwohvye.eladmin.system {
    requires transitive lwohvye.eladmin.api;
    requires transitive lwohvye.eladmin.logging;

    requires transitive java.desktop;
    requires transitive java.management;
    requires transitive java.annotation;
    requires transitive java.scripting;
    requires transitive spring.context.support;
    requires transitive spring.expression;
    requires transitive spring.websocket;
    requires transitive spring.rabbit;
    requires transitive spring.retry;
    requires transitive com.fasterxml.jackson.datatype.jsr310;
    requires transitive com.github.oshi;
    requires transitive druid;
    requires transitive easy.captcha;
    requires transitive ganymed.ssh2;
    requires transitive io.netty.common;
    requires transitive jakarta.websocket.api;
    requires transitive jjwt.api;
    requires transitive jjwt.impl;
    requires transitive jsch;
    requires transitive org.apache.commons.io;
    requires transitive quartz;
    // requires transitive jsr305; //这个里有内容与java.annotation包名重复了

    exports com.lwohvye.config.condition to spring.beans;
    exports com.lwohvye.modules.mnt.repository to spring.beans;
    exports com.lwohvye.modules.mnt.rest to spring.beans, spring.web;
    exports com.lwohvye.modules.mnt.service;
    exports com.lwohvye.modules.mnt.service.mapstruct;
    exports com.lwohvye.modules.mnt.util;
    exports com.lwohvye.modules.mnt.websocket; // 这个需要export to spring.beans和unnamed module
    exports com.lwohvye.modules.quartz.repository; // 若要使用JRebel + XRebel，则需要exports to unnamed module
    exports com.lwohvye.modules.quartz.rest to spring.beans, spring.web;
    exports com.lwohvye.modules.quartz.service;
    exports com.lwohvye.modules.quartz.task to spring.beans;
    exports com.lwohvye.modules.quartz.utils to spring.beans;
    exports com.lwohvye.modules.rabbitmq.service; // 这个要export to spring.beans和unnamed module。消费者应该都是这样的
    exports com.lwohvye.modules.security.service;
    exports com.lwohvye.modules.security.service.dto;
    exports com.lwohvye.modules.security.security;
    exports com.lwohvye.modules.security.security.filter;
    exports com.lwohvye.modules.security.security.handler;
    exports com.lwohvye.modules.security.rest to spring.beans, spring.web;
    exports com.lwohvye.modules.system.strategy;
    exports com.lwohvye.modules.system.observer;
    exports com.lwohvye.modules.system.rest to spring.beans, spring.web;
    exports com.lwohvye.modules.system.service;
    exports com.lwohvye.modules.system.service.impl to spring.beans, hutool.all;
    exports com.lwohvye.modules.system.service.local;
    exports com.lwohvye.modules.system.service.mapstruct;
    exports com.lwohvye.modules.system.subject to spring.aop, spring.core;
    exports com.lwohvye.modules.system.repository; // 这里先这样

    opens com.lwohvye.config.common;
    opens com.lwohvye.config.datasource;
    opens com.lwohvye.config.thread;
    opens com.lwohvye.modules.mnt.repository to spring.core;
    opens com.lwohvye.modules.mnt.rest to spring.core;
    opens com.lwohvye.modules.mnt.service.mapstruct;
    opens com.lwohvye.modules.mnt.service.impl;
    opens com.lwohvye.modules.mnt.websocket to spring.core;
    opens com.lwohvye.modules.quartz.config;
    opens com.lwohvye.modules.quartz.repository to spring.core;
    opens com.lwohvye.modules.quartz.rest to spring.core;
    opens com.lwohvye.modules.quartz.service.impl;
    opens com.lwohvye.modules.quartz.task to spring.core;
    opens com.lwohvye.modules.quartz.utils to spring.core;
    opens com.lwohvye.modules.rabbitmq.config;
    opens com.lwohvye.modules.rabbitmq.service to spring.core;
    opens com.lwohvye.modules.security.config;
    opens com.lwohvye.modules.security.config.bean;
    opens com.lwohvye.modules.security.service to spring.core;
    opens com.lwohvye.modules.security.rest to spring.core;
    opens com.lwohvye.modules.system.rest to spring.core;
    opens com.lwohvye.modules.system.service.impl to spring.core;
    opens com.lwohvye.modules.system.service.local;
    opens com.lwohvye.modules.system.service.mapstruct;
    opens com.lwohvye.modules.system.repository to spring.core;

    // 理论上这几个也要opens，简单而言，未open的部分是无法被外界访问的。
    opens template.email;
    opens template.generator.admin;
    opens template.generator.front;
}
