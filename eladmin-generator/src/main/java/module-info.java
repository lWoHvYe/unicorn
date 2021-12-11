@SuppressWarnings({ "requires-automatic", "requires-transitive-automatic" })
module lwohvye.eladmin.generator {
    requires transitive lwohvye.eladmin.common;
    requires transitive org.apache.commons.configuration2;

    exports com.lwohvye.generator.domain;
    exports com.lwohvye.generator.domain.vo;
    exports com.lwohvye.generator.service;
}
