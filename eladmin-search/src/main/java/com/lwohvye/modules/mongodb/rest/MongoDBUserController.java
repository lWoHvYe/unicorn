package com.lwohvye.modules.mongodb.rest;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.RandomUtil;
import com.lwohvye.annotation.AnonymousAccess;
import com.lwohvye.modules.mongodb.service.MongoDBUserService;
import org.springframework.core.io.support.SpringFactoriesLoader;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.PostConstruct;
import java.util.ServiceLoader;

/**
 * @author Hongyan Wang
 * @date 2021年04月18日 22:16
 */
@RestController
@RequestMapping("/api/mongodb/user")
public class MongoDBUserController {

    private MongoDBUserService mongoDBUserService;

    @PostConstruct
    private void doInit() {
        if (ObjectUtil.isNull(mongoDBUserService)) {
//            SPI的两种方式：Java ServiceLoader和Spring SpringFactoriesLoader。当前都无法注入属性。待解决
            if (RandomUtil.randomBoolean()) {
                var mongoDBUserServices = ServiceLoader.load(MongoDBUserService.class);
//        有值，且大小唯一
                if (ObjectUtil.isNotNull(mongoDBUserServices) && CollUtil.isNotEmpty(mongoDBUserServices) && ObjectUtil.equal(mongoDBUserServices.stream().count(), 1L)) {
                    var optionalMongoDBUserService = mongoDBUserServices.findFirst();
                    optionalMongoDBUserService.ifPresent(dbUserService -> this.mongoDBUserService = dbUserService);
                }
            } else {
                var mongoDBUserServices = SpringFactoriesLoader.loadFactories(MongoDBUserService.class, null);
//        有值，且大小唯一
                if (ObjectUtil.isNotNull(mongoDBUserServices) && CollUtil.isNotEmpty(mongoDBUserServices) && ObjectUtil.equal(mongoDBUserServices.size(), 1)) {
                    this.mongoDBUserService = mongoDBUserServices.get(0);
                }
            }
        }
    }

    @GetMapping
    @AnonymousAccess
    public ResponseEntity getAllUser() {
        return new ResponseEntity<>(mongoDBUserService.findAll(), HttpStatus.OK);
    }

    @PostMapping
    @AnonymousAccess
    public ResponseEntity updateUsers() {
        mongoDBUserService.updateUsers();
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }
}
