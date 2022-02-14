@SuppressWarnings({"requires-automatic", "requires-transitive-automatic"})
open module lwohvye.eladmin.search {// 将module设置为open，解决了未加载application.yml的问题，是因为没有opens config；在resources下的config中的配置也需要open才行。
    requires lwohvye.eladmin.system;
    requires lwohvye.eladmin.tools;
    requires lwohvye.eladmin.generator;
    requires spring.data.mongodb;

    // opens com.lwohvye;
    // opens com.lwohvye.search.modules.config;
    // 当下无法访问从BaseService继承来的的default void doInit()。自己重写自然没问题。后续排查原因。
    uses com.lwohvye.search.modules.mongodb.service.IMongoDBUserService; //声明对服务接口的使用。这个可以与接口的定义在不同的模块

    provides com.lwohvye.search.modules.mongodb.service.IMongoDBUserService with com.lwohvye.search.modules.mongodb.service.impl.MongoDBUserServiceIOCImpl; // 提供对服务接口的实现，与上面的uses可以分成不同的模块。用于替代原META-INF/services
    // 服务接口的定义、服务接口的使用uses、服务接口的提供provides xx with xx，可以在不同的模块中。并且在使用module后，原META-INF/services中的部分便忽略了。
}
