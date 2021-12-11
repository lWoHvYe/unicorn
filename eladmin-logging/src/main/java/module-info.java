@SuppressWarnings({ "requires-automatic", "requires-transitive-automatic" })
module lwohvye.eladmin.logging {
    requires transitive lwohvye.eladmin.common;

    exports com.lwohvye.log.annotation;
    exports com.lwohvye.log.domain;
    exports com.lwohvye.log.service;
    exports com.lwohvye.log.service.dto;

    opens com.lwohvye.log.annotation;
}
