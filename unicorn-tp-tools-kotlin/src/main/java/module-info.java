@SuppressWarnings({"requires-automatic"})
module lwohvye.unicorn.tp.tools.kotlin {
    requires transitive lwohvye.unicorn.core;
    requires spring.context.support;
    requires kotlin.stdlib;
    requires jakarta.mail;
    requires org.apache.logging.log4j;
    requires software.amazon.awssdk.regions;
    requires software.amazon.awssdk.services.s3;
    requires software.amazon.awssdk.transfer.s3;
    requires software.amazon.awssdk.auth;
}
