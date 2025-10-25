module lwohvye.unicorn.beans {
    requires transitive lwohvye.unicorn.core;
    requires spring.webmvc;
    requires spring.boot.persistence;
    requires spring.boot.tomcat; // do transitive on the upper layer

    exports com.lwohvye.beans.advice;
    exports com.lwohvye.beans.aspect to spring.beans, spring.aop;
    exports com.lwohvye.beans.config;
    exports com.lwohvye.beans.config.cache;
    exports com.lwohvye.beans.config.security; // opens是针对reflect，exports主要是针对import，这俩不存在包含关系
    exports com.lwohvye.beans.config.swagger;
    exports com.lwohvye.beans.mapper;
    exports com.lwohvye.beans.rabbitmq;

    opens com.lwohvye.beans.config to spring.core;
    opens com.lwohvye.beans.config.cache to spring.core;
    opens com.lwohvye.beans.rabbitmq to spring.core;
    opens com.lwohvye.beans.config.security; // 这里应该能细化，先这样粗化，后续再说
    opens com.lwohvye.beans.config.swagger to spring.core;
}
