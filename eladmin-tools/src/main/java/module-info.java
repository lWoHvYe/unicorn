module lwohvye.eladmin.tools {
    requires transitive lwohvye.eladmin.common;
    requires transitive lwohvye.eladmin.logging;
    requires java.validation;
    requires aliyun.sdk.oss;
    requires dysmsapi20170525;
    requires tea.openapi;
    requires spring.context.support;

    exports com.lwohvye.tools.utils;
}
