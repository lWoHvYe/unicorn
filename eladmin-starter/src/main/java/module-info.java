@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
module lwohvye.eladmin.starter {
    requires lwohvye.eladmin.system;
    requires lwohvye.eladmin.tools;
    requires lwohvye.eladmin.generator;

    exports com.lwohvye.starter.config to spring.beans, spring.context, spring.boot;
    exports com.lwohvye.starter.modules.service to spring.aop;
    exports com.lwohvye.starter.modules.strategy to spring.beans;

    opens config; // 注意，resources目录下的子目标并没有被open，所以需要单独open，或者直接open整个module
    opens com.lwohvye;
    opens com.lwohvye.starter.config to spring.core;
    opens com.lwohvye.starter.modules.rest;
    opens com.lwohvye.starter.modules.service.impl;
}
