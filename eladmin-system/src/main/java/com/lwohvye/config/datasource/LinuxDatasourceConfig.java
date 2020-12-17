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
import javax.sql.DataSource;
import java.util.HashMap;
import java.util.Map;

@Configuration
@EnableTransactionManagement
// TODO: 2020/12/17 不能再限制模块了。看看怎么配置
@EnableJpaRepositories(
        //实体管理
        entityManagerFactoryRef = "entityManagerFactoryLinux",
        //事务管理
        transactionManagerRef = "transactionManagerLinux",
        //实体扫描,设置Repository所在位置
        basePackages = {
                "com.lwohvye.repository.linux",
                "com.lwohvye.modules.mnt.repository.linux",
                "com.lwohvye.modules.quartz.repository.linux",
                "com.lwohvye.modules.system.repository.linux"
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
                .packages(
                        "com.lwohvye.domain.linux",
                        "com.lwohvye.modules.mnt.domain.linux",
                        "com.lwohvye.modules.quartz.domain.linux",
                        "com.lwohvye.modules.system.domain.linux"
                )
                .persistenceUnit("linuxPersistenceUnit")
                .build();
    }

    private Map<String, String> getVendorProperties() {
        Map<String, String> jpaProperties = new HashMap<>(16);
        jpaProperties.put("hibernate.hbm2ddl.auto", "none");
        jpaProperties.put("hibernate.dialect", env.getProperty("spring.jpa.hibernate.dialect"));
        jpaProperties.put("hibernate.current_session_context_class", "org.springframework.orm.hibernate5.SpringSessionContext");
        //注意在此处进行驼峰策略的配置，我也试过在yml中进行配置奈何就是不管用，可能是因为带入了多数据源的问题
        jpaProperties.put("hibernate.physical_naming_strategy", SpringPhysicalNamingStrategy.class.getName());
        jpaProperties.put("hibernate.implicit_naming_strategy", SpringImplicitNamingStrategy.class.getName());
        return jpaProperties;
    }

    @Bean(name = "transactionManagerLinux")
    PlatformTransactionManager transactionManagerLinux(EntityManagerFactoryBuilder builder) {
        return new JpaTransactionManager(entityManagerFactoryLinux(builder).getObject());
    }
}
