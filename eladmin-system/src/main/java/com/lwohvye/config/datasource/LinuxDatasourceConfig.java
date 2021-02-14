package com.lwohvye.config.datasource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.orm.jpa.EntityManagerFactoryBuilder;
import org.springframework.boot.orm.jpa.hibernate.SpringImplicitNamingStrategy;
import org.springframework.boot.orm.jpa.hibernate.SpringPhysicalNamingStrategy;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;
import java.util.HashMap;
import java.util.Map;

@Configuration
@EnableTransactionManagement
// 从数据源的repository统一放在 com.lwohvye.modules.linux 包下
@EnableJpaRepositories(
        //实体管理
        entityManagerFactoryRef = "entityManagerFactoryLinux",
        //事务管理
        transactionManagerRef = "transactionManagerLinux",
        //实体扫描,设置Repository所在位置。每个数据源唯一
        basePackages = {
                "com.lwohvye.linux",
                "com.lwohvye.modules.linux"
        })
public class LinuxDatasourceConfig {

    @Autowired
    @Qualifier("linuxDataSource")
    private DataSource linuxDataSource;

    @Autowired
    private Environment env;

    @Bean(name = "entityManagerSecond")
    public EntityManager entityManager(EntityManagerFactoryBuilder builder) {
        return entityManagerFactoryLinux(builder).getObject().createEntityManager();
    }

    @Bean(name = "entityManagerFactoryLinux")
    public LocalContainerEntityManagerFactoryBean entityManagerFactoryLinux(EntityManagerFactoryBuilder builder) {
        return builder
                .dataSource(linuxDataSource)
                .properties(getVendorProperties())
//                配置实体所在的位置。多数据源可共用
//                需要分包时，包名为com.lwohvye.modules.linux.xxxx.domain
//                不需要分包时，包名为com.lwohvye.modules.domain。使用其他的可能扫描不到
                .packages(
                        "com.lwohvye.domain",
                        "com.lwohvye.modules.mnt.domain",
                        "com.lwohvye.modules.quartz.domain",
                        "com.lwohvye.modules.system.domain",
                        "com.lwohvye.modules.linux"
                )
                .persistenceUnit("linuxPersistenceUnit")
                .build();
    }

    private Map<String, String> getVendorProperties() {
        Map<String, String> jpaProperties = new HashMap<>(16);
        jpaProperties.put("hibernate.hbm2ddl.auto", "none");
        jpaProperties.put("hibernate.dialect", env.getProperty("spring.jpa.hibernate.dialect"));
        jpaProperties.put("hibernate.current_session_context_class", "org.springframework.orm.hibernate5.SpringSessionContext");
        jpaProperties.put("hibernate.enable_lazy_load_no_trans", env.getProperty("spring.jpa.hibernate.enable_lazy_load_no_trans"));
        //注意在此处进行驼峰策略的配置，我也试过在yml中进行配置奈何就是不管用，可能是因为带入了多数据源的问题
        jpaProperties.put("hibernate.physical_naming_strategy", SpringPhysicalNamingStrategy.class.getName());
        jpaProperties.put("hibernate.implicit_naming_strategy", SpringImplicitNamingStrategy.class.getName());
        return jpaProperties;
    }

    @Bean(name = "transactionManagerLinux")
    PlatformTransactionManager transactionManagerLinux(@Qualifier("entityManagerFactoryLinux") EntityManagerFactory entityManagerFactory) {
        return new JpaTransactionManager(entityManagerFactory);
    }
}
