@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
module lwohvye.eladmin.starter {
    requires lwohvye.eladmin.system;
    requires lwohvye.eladmin.tools;
    requires lwohvye.eladmin.generator;

    opens config; // 注意，resources目录应该默认被open了，但其下的子目标并没有，所以需要单独open，或者直接open整个module
    opens com.lwohvye;
}
