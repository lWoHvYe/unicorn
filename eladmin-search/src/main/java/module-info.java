@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
open module lwohvye.eladmin.search {// 将module设置为open，解决了未加载application.yml配置的问题，原因不清楚
    requires lwohvye.eladmin.system;
    requires lwohvye.eladmin.tools;
    requires lwohvye.eladmin.generator;
    requires spring.data.mongodb;

    // opens com.lwohvye;
    // opens com.lwohvye.search.modules.config;

    uses com.lwohvye.search.modules.mongodb.service.IMongoDBUserService;

    provides com.lwohvye.search.modules.mongodb.service.IMongoDBUserService with com.lwohvye.search.modules.mongodb.service.impl.MongoDBUserServiceIOCImpl;
}
