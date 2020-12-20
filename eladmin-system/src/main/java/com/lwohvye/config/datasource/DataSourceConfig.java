package com.lwohvye.config.datasource;

import com.alibaba.druid.pool.DruidDataSource;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import javax.sql.DataSource;

@Configuration
public class DataSourceConfig {

    /**
     * 第一个数据连接，默认优先级最高
     * @return
     */
    @Primary
    @Bean(name = "mainDataSource")
    @Qualifier("mainDataSource")
    @ConfigurationProperties(prefix="spring.datasource.druid.main")
    public DataSource mainDataSource() {
        return DataSourceBuilder.create().type(DruidDataSource.class).build();
    }

    /**
     * 第二个数据源
     * @return
     */
    @Bean(name = "linuxDataSource")
    @Qualifier("linuxDataSource")
    @ConfigurationProperties(prefix="spring.datasource.druid.linux")
    public DataSource linuxDataSource() {
        return DataSourceBuilder.create().type(DruidDataSource.class).build();
    }
}
