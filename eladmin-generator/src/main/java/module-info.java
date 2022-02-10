@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
module lwohvye.eladmin.generator {
    requires transitive lwohvye.eladmin.common;
    requires transitive org.apache.commons.configuration2;

    exports com.lwohvye.generator.domain;
    exports com.lwohvye.generator.domain.vo;
    exports com.lwohvye.generator.repository to spring.beans;
    exports com.lwohvye.generator.rest to spring.beans, spring.web;
    exports com.lwohvye.generator.service;
    exports com.lwohvye.generator.service.impl to spring.beans;

    opens com.lwohvye.generator.domain;
    opens com.lwohvye.generator.repository to spring.core;
    opens com.lwohvye.generator.rest to spring.core;
    opens com.lwohvye.generator.service.impl to spring.core;
}
