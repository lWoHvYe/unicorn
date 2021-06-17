package com.lwohvye.base;

import com.lwohvye.config.LocalCoreConfig;

public interface BaseService {

    default String getSysName() {
        return LocalCoreConfig.getSysName();
    }
}
