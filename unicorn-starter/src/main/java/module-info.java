@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
module lwohvye.unicorn.starter {
    requires lwohvye.unicorn.system;
    requires lwohvye.unicorn.tp.tools;
    requires lwohvye.unicorn.code.gen;
    requires org.apache.httpcomponents.httpcore;
    requires org.apache.httpcomponents.httpclient;
    requires kotlin.stdlib;
    requires kotlinx.coroutines.core.jvm;
    // 如果使用3rd-tools，需要加入下面这两个，不清楚为何在tools中加没生效
    requires jakarta.mail;
    requires jakarta.activation;

    exports com.lwohvye.starter.config to spring.beans, spring.context, spring.boot;
    exports com.lwohvye.starter.modules.service to spring.aop;
    exports com.lwohvye.starter.modules.strategy to spring.beans; // 全是kt的package无法exports，算是bug吧

    opens config; // 注意，resources目录下的子目标并没有被open，所以需要单独open，或者直接open整个module
    opens com.lwohvye;
    opens com.lwohvye.starter.config to spring.core;
    opens com.lwohvye.starter.modules.rest;
    opens com.lwohvye.starter.modules.service.impl;
}
