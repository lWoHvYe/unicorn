package com.lwohvye.modules.elasticsearch.rest;

import com.lwohvye.annotation.rest.AnonymousGetMapping;
import com.lwohvye.modules.elasticsearch.service.EsUserService;
import com.lwohvye.utils.result.ResultInfo;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping("/api/es/user")
@RequiredArgsConstructor
public class EsUserController {

    private final EsUserService esUserService;

    @AnonymousGetMapping
    public ResponseEntity getAllEsUser() {
        return new ResponseEntity<>(ResultInfo.success(esUserService.queryAll()), HttpStatus.OK);
    }

    public ResponseEntity updateUsers() {
        esUserService.updateUsers();
        return new ResponseEntity<>(ResultInfo.success(), HttpStatus.NO_CONTENT);
    }

}
