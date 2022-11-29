@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
module lwohvye.unicorn.logging {
    requires transitive lwohvye.unicorn.core;
    requires transitive spring.rabbit;

    exports com.lwohvye.log.aspect to spring.beans, spring.aop;
    exports com.lwohvye.log.domain;
    exports com.lwohvye.log.rabbitmq;
    exports com.lwohvye.log.repository to spring.beans;
    exports com.lwohvye.log.rest to spring.beans, spring.aop, spring.web;
    exports com.lwohvye.log.service;
    exports com.lwohvye.log.service.dto;
    exports com.lwohvye.log.service.impl to spring.beans;
    exports com.lwohvye.log.service.mapstruct;

    opens com.lwohvye.log.domain;
    opens com.lwohvye.log.rabbitmq to spring.core;
    opens com.lwohvye.log.repository to spring.core;
    opens com.lwohvye.log.rest to spring.core;
    opens com.lwohvye.log.service.impl to spring.core;
    opens com.lwohvye.log.service.local;
    opens com.lwohvye.log.service.mapstruct;
}
