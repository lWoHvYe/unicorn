package com.lwohvye.config.datasource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.orm.jpa.EntityManagerFactoryBuilder;
import org.springframework.boot.orm.jpa.hibernate.SpringImplicitNamingStrategy;
import org.springframework.boot.orm.jpa.hibernate.SpringPhysicalNamingStrategy;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
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
import java.util.Objects;

@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(
        entityManagerFactoryRef = "entityManagerFactoryMain",
        transactionManagerRef = "transactionManagerMain",
        basePackages = {
                "com.lwohvye.main",
                "com.lwohvye.modules.main"
        })
public class MainDataSourceConfig {

    @Autowired
    private Environment env;

    @Autowired
    @Qualifier("mainDataSource")
    private DataSource mainDataSource;

    @Primary
    @Bean(name = "entityManagerMain")
    public EntityManager entityManager(EntityManagerFactoryBuilder builder) {
        return entityManagerFactoryMain(builder).getObject().createEntityManager();
    }

    @Primary
    @Bean(name = "entityManagerFactoryMain")
    public LocalContainerEntityManagerFactoryBean entityManagerFactoryMain(EntityManagerFactoryBuilder builder) {
        return builder
                .dataSource(mainDataSource)
                .properties(getVendorProperties())
                .packages(
                        "com.lwohvye.domain",
                        "com.lwohvye.modules.mnt.domain",
                        "com.lwohvye.modules.quartz.domain",
                        "com.lwohvye.modules.system.domain",
                        "com.lwohvye.main",
                        "com.lwohvye.modules.main"
                )
                .persistenceUnit("mainPersistenceUnit")
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

    @Primary
    @Bean(name = "transactionManagerMain")
    public PlatformTransactionManager transactionManagerMain(@Qualifier("entityManagerFactoryMain") EntityManagerFactory entityManagerFactory) {
        return new JpaTransactionManager(entityManagerFactory);
    }
}
