@SuppressWarnings({ "requires-automatic", "requires-transitive-automatic" })
module lwohvye.unicorn.sys.api {
    requires transitive lwohvye.unicorn.core;

    exports com.lwohvye.api.modules.system.api;
    exports com.lwohvye.api.modules.system.domain;
    exports com.lwohvye.api.modules.system.domain.vo;
    exports com.lwohvye.api.modules.system.domain.projection;
    exports com.lwohvye.api.modules.system.service.dto;

    opens com.lwohvye.api.modules.system.domain;
    opens com.lwohvye.api.modules.system.domain.vo;
    opens com.lwohvye.api.modules.system.domain.projection;
    opens com.lwohvye.api.modules.system.service.dto;

}
