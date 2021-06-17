package com.lwohvye.base;

import com.lwohvye.config.LocalCoreConfig;

/**
 * @author Hongyan Wang
 * @description Service层部分公共方法
 * @date 2021/6/17 5:07 下午
 */
public interface BaseService {

    //接口中可以有静态方法、默认方法、私有方法
    default String getSysName() {
        return LocalCoreConfig.getSysName();
    }
}
