@SuppressWarnings({ "requires-automatic", "requires-transitive-automatic" })
module lwohvye.eladmin.api {
    requires transitive lwohvye.eladmin.common;

    exports com.lwohvye.api.annotation;
    exports com.lwohvye.api.utils;
    exports com.lwohvye.api.modules.mnt.domain;
    exports com.lwohvye.api.modules.mnt.service.dto;
    exports com.lwohvye.api.modules.quartz.domain;
    exports com.lwohvye.api.modules.quartz.service.dto;
    exports com.lwohvye.api.modules.system.api;
    exports com.lwohvye.api.modules.system.domain;
    exports com.lwohvye.api.modules.system.domain.vo;
    exports com.lwohvye.api.modules.system.domain.projection;
    exports com.lwohvye.api.modules.system.service.dto;

    opens com.lwohvye.api.modules.mnt.domain;
    opens com.lwohvye.api.modules.mnt.service.dto;
    opens com.lwohvye.api.modules.quartz.domain;
    opens com.lwohvye.api.modules.quartz.service.dto;
    opens com.lwohvye.api.modules.system.domain;
    opens com.lwohvye.api.modules.system.domain.vo;
    opens com.lwohvye.api.modules.system.domain.projection;
    opens com.lwohvye.api.modules.system.service.dto;

}
