@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
module lwohvye.eladmin.tools {
    requires transitive lwohvye.eladmin.common;
    requires transitive aliyun.sdk.oss;
    requires transitive dysmsapi20170525;
    requires transitive spring.context.support;
    requires transitive tea.openapi;

    exports com.lwohvye.tools.config to spring.beans, spring.context;
    exports com.lwohvye.tools.domain.vo;
    exports com.lwohvye.tools.repository to spring.beans;
    exports com.lwohvye.tools.rest to spring.beans, spring.web;
    exports com.lwohvye.tools.service;
    exports com.lwohvye.tools.service.dto;
    exports com.lwohvye.tools.service.impl;
    exports com.lwohvye.tools.service.mapstruct;
    exports com.lwohvye.tools.utils;

    opens com.lwohvye.tools.config to spring.core;
    opens com.lwohvye.tools.domain;
    opens com.lwohvye.tools.repository to spring.core;
    opens com.lwohvye.tools.rest to spring.core;
    opens com.lwohvye.tools.service to spring.core;
    opens com.lwohvye.tools.service.impl to spring.core;
    opens com.lwohvye.tools.service.mapstruct;
    opens com.lwohvye.tools.utils to spring.core;
}
