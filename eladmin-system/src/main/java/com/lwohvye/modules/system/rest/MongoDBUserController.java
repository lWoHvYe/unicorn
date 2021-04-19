package com.lwohvye.modules.system.rest;

import com.lwohvye.annotation.AnonymousAccess;
import com.lwohvye.modules.system.service.MongoDBUserService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Hongyan Wang
 * @date 2021年04月18日 22:16
 */
@Deprecated
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/mongodb/user")
public class MongoDBUserController {

    private final MongoDBUserService mongoDBUserService;

    @GetMapping
    @AnonymousAccess
    public ResponseEntity getAllUser(){
        return new ResponseEntity<>(mongoDBUserService.findAll(), HttpStatus.OK);
    }

    @PostMapping
    @AnonymousAccess
    public ResponseEntity updateUsers(){
        mongoDBUserService.updateUsers();
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }
}
