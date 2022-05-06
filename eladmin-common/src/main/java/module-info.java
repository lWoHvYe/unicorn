// 暂作为open module。允许其他模块通过反射访问，后续缩小范围
@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
        // 抑制compile warn: requires transitive directive for an automatic module
module lwohvye.eladmin.common {
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
    requires transitive spring.webmvc;
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
    requires transitive mica.ip2region;
    requires transitive nl.basjes.parse.useragent;
    requires transitive org.apache.commons.codec;
    requires transitive org.apache.commons.lang3;
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
    requires transitive thumbnailator;

    exports com.lwohvye.advice;
    exports com.lwohvye.annotation;
    exports com.lwohvye.annotation.log;
    exports com.lwohvye.annotation.rest;
    exports com.lwohvye.aop to spring.beans, spring.aop;
    exports com.lwohvye.aspect to spring.beans, spring.aop;
    exports com.lwohvye.base;
    exports com.lwohvye.config;
    exports com.lwohvye.config.cache;
    exports com.lwohvye.config.security; // open未完全包含exports，至少import需要export
    exports com.lwohvye.config.swagger;
    exports com.lwohvye.constant;
    exports com.lwohvye.context;
    exports com.lwohvye.exception;
    exports com.lwohvye.exception.handler;
    exports com.lwohvye.utils;
    exports com.lwohvye.utils.enums;
    exports com.lwohvye.utils.json;
    exports com.lwohvye.utils.mapper;
    exports com.lwohvye.utils.rabbitmq;
    exports com.lwohvye.utils.redis;
    exports com.lwohvye.utils.result;

    opens com.lwohvye.base;
    opens com.lwohvye.config to spring.core;
    opens com.lwohvye.config.cache to spring.core;
    opens com.lwohvye.config.security; // 这里应该能细化，先这样粗化，后续再说
    opens com.lwohvye.config.swagger to spring.core;
    opens com.lwohvye.utils to spring.core;
    opens com.lwohvye.utils.json to spring.core;
    opens com.lwohvye.utils.rabbitmq to spring.core;
    opens com.lwohvye.utils.redis to spring.core;
    opens com.lwohvye.utils.result to spring.core;
}
