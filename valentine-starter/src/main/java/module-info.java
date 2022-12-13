@SuppressWarnings({"requires-automatic"})
module lwohvye.valentine.starter {
    requires lwohvye.unicorn.system;
    requires lwohvye.unicorn.core;
    requires lombok;
//    requires bizlog.sdk;
//    requires captcha;

    exports com.unicorn.vs.rest to spring.beans, spring.aop, spring.web;

//    opens config; // 注意，resources目录下的子目标并没有被open，所以需要单独open，或者直接open整个module
    opens com.unicorn;
    opens com.unicorn.vs.rest to spring.core;
}
