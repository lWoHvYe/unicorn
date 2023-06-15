@SuppressWarnings({"requires-automatic"})
module lwohvye.unicorn.tp.tools {
    requires transitive lwohvye.unicorn.core;
    requires spring.context.support;
    requires kotlin.stdlib;
    requires jakarta.mail;
    requires software.amazon.awssdk.regions;
    requires software.amazon.awssdk.services.s3;
    requires software.amazon.awssdk.transfer.s3;
    requires software.amazon.awssdk.auth;
}
