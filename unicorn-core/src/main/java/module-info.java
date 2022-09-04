// 暂作为open module。允许其他模块通过反射访问，后续缩小范围
@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
        // 抑制compile warn: requires transitive directive for an automatic module
module lwohvye.unicorn.core {
    requires transitive java.compiler;
    requires transitive java.servlet;
    requires transitive java.sql;
    requires transitive java.persistence;
    requires transitive java.validation;
    requires transitive jdk.unsupported;
    requires transitive spring.amqp;
    requires transitive spring.core;
    requires transitive spring.beans;
    requires transitive spring.context;
    requires transitive spring.tx;
    requires transitive spring.aop;
    requires transitive spring.aspects;
    requires transitive spring.web;
    requires spring.webmvc; // do transitive on the upper layer
    requires transitive spring.jdbc;
    requires transitive spring.data.commons;
    requires transitive spring.data.jpa;
    requires transitive spring.data.redis;
    requires transitive spring.security.config;
    requires transitive spring.security.core;
    requires transitive spring.security.crypto;
    requires transitive spring.security.web;
    requires transitive spring.boot;
    requires transitive spring.boot.autoconfigure;
    requires transitive com.fasterxml.jackson.annotation;
    requires transitive com.fasterxml.jackson.databind;
    requires transitive com.github.benmanes.caffeine;
    requires transitive hutool.all;
    requires transitive io.swagger.v3.oas.models;
    requires transitive io.swagger.v3.oas.annotations;
    requires transitive lombok;
    requires mica.ip2region;
    requires nl.basjes.parse.useragent;
    requires org.apache.commons.codec;
    requires org.apache.commons.lang3;
    requires transitive org.apache.poi.poi;
    requires transitive org.apache.poi.ooxml;
    requires transitive org.aspectj.weaver;
    requires transitive com.google.common;
    requires transitive org.hibernate.orm.core;
    requires transitive org.hibernate.validator;
    requires transitive org.jetbrains.annotations;
    requires transitive org.mapstruct;
    requires transitive mapstruct.spring.extensions;
    requires transitive org.slf4j;
    requires transitive org.springdoc.openapi.common;
    requires transitive redisson;
    requires thumbnailator;

    exports com.lwohvye.core.advice;
    exports com.lwohvye.core.annotation;
    exports com.lwohvye.core.annotation.log;
    exports com.lwohvye.core.annotation.rest;
    exports com.lwohvye.core.aop to spring.beans, spring.aop;
    exports com.lwohvye.core.aspect to spring.beans, spring.aop;
    exports com.lwohvye.core.base;
    exports com.lwohvye.core.config;
    exports com.lwohvye.core.config.cache;
    exports com.lwohvye.core.config.security; // open未完全包含exports，至少import需要export
    exports com.lwohvye.core.config.swagger;
    exports com.lwohvye.core.constant;
    exports com.lwohvye.core.context;
    exports com.lwohvye.core.exception;
    exports com.lwohvye.core.exception.handler;
    exports com.lwohvye.core.utils;
    exports com.lwohvye.core.utils.enums;
    exports com.lwohvye.core.utils.json;
    exports com.lwohvye.core.utils.mapper;
    exports com.lwohvye.core.utils.rabbitmq;
    exports com.lwohvye.core.utils.redis;
    exports com.lwohvye.core.utils.result;

    opens com.lwohvye.core.base;
    opens com.lwohvye.core.config to spring.core;
    opens com.lwohvye.core.config.cache to spring.core;
    opens com.lwohvye.core.config.security; // 这里应该能细化，先这样粗化，后续再说
    opens com.lwohvye.core.config.swagger to spring.core;
    opens com.lwohvye.core.utils to spring.core;
    opens com.lwohvye.core.utils.json to spring.core;
    opens com.lwohvye.core.utils.rabbitmq to spring.core;
    opens com.lwohvye.core.utils.redis to spring.core;
    opens com.lwohvye.core.utils.result to spring.core;
}
