// 暂作为open module。允许其他模块通过反射访问，后续缩小范围
module lwohvye.eladmin.common {
    requires transitive java.servlet;
    requires transitive java.sql;
    requires transitive java.persistence;
    requires transitive spring.core;
    requires transitive spring.beans;
    requires transitive spring.context;
    requires transitive spring.tx;
    requires transitive spring.web;
    requires transitive spring.webmvc;
    requires transitive spring.jdbc;
    requires transitive spring.data.commons;
    requires transitive spring.data.jpa;
    requires transitive spring.data.redis;
    requires transitive spring.security.core;
    requires transitive spring.boot;
    requires transitive spring.boot.autoconfigure;
    requires transitive lombok;
    requires transitive org.aspectj.weaver;
    requires transitive org.slf4j;
    requires transitive com.google.common;
    requires transitive org.apache.commons.codec;
    requires transitive org.apache.commons.lang3;
    requires transitive org.hibernate.orm.core;
    requires transitive org.mapstruct;
    requires transitive hutool.all;
    requires transitive io.swagger.v3.oas.models;
    requires transitive io.swagger.v3.oas.annotations;
    requires transitive org.springdoc.openapi.common;
    requires transitive com.fasterxml.jackson.annotation;
    requires transitive com.fasterxml.jackson.databind;
    requires transitive org.apache.poi.poi;
    requires transitive org.apache.poi.ooxml;
    requires transitive mica.ip2region;
    requires transitive nl.basjes.parse.useragent;
    requires transitive org.hibernate.validator;

    exports com.lwohvye.advice;
    exports com.lwohvye.annotation;
    exports com.lwohvye.annotation.rest;
    exports com.lwohvye.base;
    exports com.lwohvye.config;
    exports com.lwohvye.constant;
    exports com.lwohvye.context;
    exports com.lwohvye.exception;
    exports com.lwohvye.exception.handler;
    exports com.lwohvye.utils;
    exports com.lwohvye.utils.enums;
    exports com.lwohvye.utils.json;
    exports com.lwohvye.utils.mapper;
    exports com.lwohvye.utils.redis;
    exports com.lwohvye.utils.result;

    opens com.lwohvye.base;
}
