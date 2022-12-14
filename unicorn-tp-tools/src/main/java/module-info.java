@SuppressWarnings({"requires-automatic"})
module lwohvye.unicorn.tp_tools {
    requires transitive lwohvye.unicorn.core;
    requires spring.context.support;
    requires jakarta.mail;
    requires software.amazon.awssdk.regions;
    requires software.amazon.awssdk.services.s3;
    requires software.amazon.awssdk.transfer.s3;
    requires software.amazon.awssdk.auth;

    exports com.lwohvye.tools.config to spring.beans, spring.context, spring.boot;
    exports com.lwohvye.tools.domain.vo;
    exports com.lwohvye.tools.repository to spring.beans;
    exports com.lwohvye.tools.rest to spring.beans, spring.aop, spring.web;
    exports com.lwohvye.tools.service;
    exports com.lwohvye.tools.service.impl;
    exports com.lwohvye.tools.utils;

    opens com.lwohvye.tools.config to spring.core;
    opens com.lwohvye.tools.domain;
    opens com.lwohvye.tools.repository to spring.core;
    opens com.lwohvye.tools.rest to spring.core;
    opens com.lwohvye.tools.service to spring.core;
    opens com.lwohvye.tools.service.impl to spring.core;
    opens com.lwohvye.tools.utils to spring.core;
}
