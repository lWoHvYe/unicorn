package com.lwohvye.modules.elasticsearch.service;

import com.lwohvye.base.BaseService;

public interface IEsUserService extends BaseService {

    Object queryAll();

    void updateUsers();
}
