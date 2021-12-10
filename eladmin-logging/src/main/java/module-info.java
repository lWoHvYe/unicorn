module lwohvye.eladmin.logging {
    requires transitive lwohvye.eladmin.common;

    exports com.lwohvye.log.annotation;
    exports com.lwohvye.domain;
    exports com.lwohvye.service;
    exports com.lwohvye.service.dto;

    opens com.lwohvye.log.annotation;
}
