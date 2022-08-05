@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
module lwohvye.valentine.starter {
    requires lwohvye.eladmin.system;
    requires kotlin.stdlib;
    requires kotlinx.coroutines.core.jvm;

    opens config; // 注意，resources目录下的子目标并没有被open，所以需要单独open，或者直接open整个module
    opens com.lwohvye;
}
