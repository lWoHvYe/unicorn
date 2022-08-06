// 暂作为open module。允许其他模块通过反射访问，后续缩小范围
@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
        // 抑制compile warn: requires transitive directive for an automatic module
module lwohvye.unicorn.common_webflux {
    requires transitive lwohvye.unicorn.common;
    requires transitive reactor.core;

    exports com.lwohvye.reactive.config.security; // open未完全包含exports，至少import需要export

    opens com.lwohvye.reactive.config.security; // 这里应该能细化，先这样粗化，后续再说
}
