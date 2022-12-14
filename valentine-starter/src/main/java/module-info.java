@SuppressWarnings({"requires-automatic"})
module lwohvye.valentine.starter {
    requires lwohvye.unicorn.system;
    requires lwohvye.unicorn.core;
    requires lwohvye.unicorn.tp_tools;
    requires lombok;
    requires com.mzt.logapi;
    // 如果使用3rd-tools，需要加入下面这两个，不清楚为何在tools中加没生效。mail works well under unnamed module
    requires jakarta.mail;
    requires jakarta.activation;
//    requires bizlog.sdk;
//    requires captcha;

    exports com.unicorn.vs.rest to spring.beans, spring.aop, spring.web;

    // maven需要opens resources，而gradle不需要
//    opens config;
    opens com.unicorn;
    opens com.unicorn.vs.rest to spring.core;
}
