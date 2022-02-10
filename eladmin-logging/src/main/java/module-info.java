@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
module lwohvye.eladmin.logging {
    requires transitive lwohvye.eladmin.common;

    exports com.lwohvye.log.annotation;
    exports com.lwohvye.log.aspect to spring.beans;
    exports com.lwohvye.log.domain;
    exports com.lwohvye.log.repository to spring.beans;
    exports com.lwohvye.log.rest to spring.beans, spring.web;
    exports com.lwohvye.log.service;
    exports com.lwohvye.log.service.dto;
    exports com.lwohvye.log.service.impl to spring.beans;
    exports com.lwohvye.log.service.mapstruct;

    opens com.lwohvye.log.annotation;
    opens com.lwohvye.log.domain;
    opens com.lwohvye.log.repository to spring.core;
    opens com.lwohvye.log.rest to spring.core;
    opens com.lwohvye.log.service.impl to spring.core;
    opens com.lwohvye.log.service.mapstruct;
}
