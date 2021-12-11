@SuppressWarnings({ "requires-automatic", "requires-transitive-automatic" })
module lwohvye.eladmin.tools {
    requires transitive lwohvye.eladmin.common;
    requires transitive lwohvye.eladmin.logging;
    requires transitive aliyun.sdk.oss;
    requires transitive dysmsapi20170525;
    requires transitive tea.openapi;
    requires transitive spring.context.support;

    exports com.lwohvye.tools.utils;
    exports com.lwohvye.tools.domain.vo;
    exports com.lwohvye.tools.service;
}
