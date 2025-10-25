@SuppressWarnings({"requires-automatic"})
module lwohvye.valentine.starter {
    requires lwohvye.unicorn.security;
    requires lwohvye.unicorn.tp.tools.kotlin;
    requires lombok;
    // 如果使用3rd-tools，需要加入下面这两个，不清楚为何在tools中加没生效。mail works well under unnamed module
    requires jakarta.mail;
    requires jakarta.activation;
    requires bizlog.sdk;
    requires kotlin.stdlib;
    requires kotlin.reflect;
    requires kotlinx.coroutines.core;
    requires org.apache.logging.log4j;
    requires org.apache.httpcomponents.core5.httpcore5;
    requires org.apache.httpcomponents.client5.httpclient5;

    opens com.unicorn;
}
