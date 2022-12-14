@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
module lwohvye.unicorn.cd_generator {
    requires transitive lwohvye.unicorn.core;
    requires org.apache.commons.configuration2;

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

    // maven需要opens resources，而gradle不需要
//    opens template.generator.admin;
//    opens template.generator.front;
}
