module lwohvye.eladmin.api {
    requires transitive lwohvye.eladmin.common;
    requires transitive java.validation;

    exports com.lwohvye.api.annotation;
    exports com.lwohvye.api.utils;
    exports com.lwohvye.modules.mnt.domain;
    exports com.lwohvye.modules.mnt.service;
    exports com.lwohvye.modules.mnt.service.dto;
    exports com.lwohvye.modules.quartz.domain;
    exports com.lwohvye.modules.quartz.service;
    exports com.lwohvye.modules.quartz.service.dto;
    exports com.lwohvye.modules.system.domain;
    exports com.lwohvye.modules.system.domain.vo;
    exports com.lwohvye.modules.system.domain.projection;
    exports com.lwohvye.modules.system.service;
    exports com.lwohvye.modules.system.service.dto;

    opens com.lwohvye.modules.mnt.domain;
    opens com.lwohvye.modules.mnt.service.dto;
    opens com.lwohvye.modules.quartz.domain;
    opens com.lwohvye.modules.quartz.service.dto;
    opens com.lwohvye.modules.system.domain;
    opens com.lwohvye.modules.system.service.dto;

}
