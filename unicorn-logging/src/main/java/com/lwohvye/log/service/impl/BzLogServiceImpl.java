/*
 *  Copyright 2019-2020 Zheng Jie
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.lwohvye.log.service.impl;

import cn.hutool.core.lang.Dict;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.json.JSONUtil;
import com.lwohvye.core.annotation.log.OprLog;
import com.lwohvye.core.utils.*;
import com.lwohvye.log.domain.BzLog;
import com.lwohvye.log.repository.BzLogRepository;
import com.lwohvye.log.service.IBzLogService;
import com.lwohvye.log.service.dto.BzLogErrorDTO;
import com.lwohvye.log.service.dto.BzLogQueryCriteria;
import com.lwohvye.log.service.dto.BzLogSmallDTO;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.lang.reflect.Method;
import java.util.*;

/**
 * @author Zheng Jie
 * @date 2018-11-24
 */
@Slf4j
@Service
//有参构造
@RequiredArgsConstructor
public class BzLogServiceImpl implements IBzLogService {
    private final BzLogRepository bzLogRepository;

    private final ConversionService conversionService;

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public Map<String, Object> queryAll(BzLogQueryCriteria criteria, Pageable pageable) {
        Page<BzLog> page = bzLogRepository.findAll(((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder)), pageable);
        String status = "ERROR";
        if (status.equals(criteria.getLogType())) {
            return PageUtils.toPage(page.map(errInfo -> conversionService.convert(errInfo, BzLogErrorDTO.class)));
        }
        return PageUtils.toPage(page);
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public List<BzLog> queryAll(BzLogQueryCriteria criteria) {
        return bzLogRepository.findAll(((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder)));
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public Map<String, Object> queryAllByUser(BzLogQueryCriteria criteria, Pageable pageable) {
        Page<BzLog> page = bzLogRepository.findAll(((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder)), pageable);
        return PageUtils.toPage(page.map(logInfo -> conversionService.convert(logInfo, BzLogSmallDTO.class)));
    }

    /**
     * 日志为异步写的。不影响相关的业务
     *
     * @params [username, browser, ip, joinPoint, log]
     * @date 2021/3/25 23:39
     */
    @Async
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(String username, String browser, String ip, ProceedingJoinPoint joinPoint, BzLog opBzLog) {

        var signature = (MethodSignature) joinPoint.getSignature();
        var method = signature.getMethod();
        var aopLog = method.getAnnotation(OprLog.class);

        // 方法路径
        var methodName = STR."\{joinPoint.getTarget().getClass().getName()}.\{signature.getName()}()";

        // 描述
        if (opBzLog != null) {
            opBzLog.setDescription(aopLog.value());
        }
        Assert.notNull(opBzLog, "信息有误，不可为空");
        opBzLog.setRequestIp(ip);

        opBzLog.setAddress(StringUtils.getHttpCityInfo(opBzLog.getRequestIp()));
        opBzLog.setMethod(methodName);
        opBzLog.setUsername(username);
        opBzLog.setParams(getParameter(method, joinPoint.getArgs()));
        opBzLog.setBrowser(browser);
        bzLogRepository.save(opBzLog);
    }

    @Async
    @Retryable(retryFor = RuntimeException.class, backoff = @Backoff(delay = 3200, multiplier = 2))
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(BzLog resource) {
        bzLogRepository.save(resource);
    }

    /**
     * 根据方法和传入的参数获取请求参数.
     * 虽然尴尬，但我们通过反射只能拿到方法签名和参数列表，签名中每个参数的具体含义是不知道的
     */
    private String getParameter(Method method, Object[] args) {
        var argList = new ArrayList<>();
        var parameters = method.getParameters();
        for (int i = 0; i < parameters.length; i++) {
            //将RequestBody注解修饰的参数作为请求参数
            var requestBody = parameters[i].getAnnotation(RequestBody.class);
            if (requestBody != null) {
                argList.add(args[i]);
            }
            //将RequestParam注解修饰的参数作为请求参数
            var requestParam = parameters[i].getAnnotation(RequestParam.class);
            if (requestParam != null) {
                var map = new HashMap<>();
                var key = parameters[i].getName();
                if (!StringUtils.isEmpty(requestParam.value())) {
                    key = requestParam.value();
                }
                map.put(key, args[i]);
                argList.add(map);
            }
        }
        if (argList.isEmpty()) {
            return "";
        }
        return argList.size() == 1 ? JSONUtil.toJsonStr(argList.getFirst()) : JSONUtil.toJsonStr(argList);
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public Dict findByErrDetail(Long id) {
        BzLog bzLog = bzLogRepository.findById(id).orElseGet(BzLog::new);
        ValidationUtils.isNull(bzLog.getId(), "Log", "id", id);
        byte[] details = bzLog.getExceptionDetail();
        return Dict.create().set("exception", new String(ObjectUtil.isNotNull(details) ? details : "".getBytes()));
    }

    @Override
    public void download(List<BzLog> opBzLogs, HttpServletResponse response) throws IOException {
        List<Map<String, Object>> list = new ArrayList<>();
        for (BzLog bzLog : opBzLogs) {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("用户名", bzLog.getUsername());
            map.put("IP", bzLog.getRequestIp());
            map.put("IP来源", bzLog.getAddress());
            map.put("描述", bzLog.getDescription());
            map.put("浏览器", bzLog.getBrowser());
            map.put("请求耗时/毫秒", bzLog.getTime());
            map.put("异常详情", new String(ObjectUtil.isNotNull(bzLog.getExceptionDetail()) ? bzLog.getExceptionDetail() : "".getBytes()));
            map.put("创建日期", bzLog.getCreateTime());
            list.add(map);
        }
        FileUtils.downloadExcel(list, response);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delAllByError() {
        bzLogRepository.deleteByLogType("ERROR");
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delAllByInfo() {
        bzLogRepository.deleteByLogType("INFO");
    }

    @Recover
    public void saveRecover(RuntimeException e, BzLog resource) {
        log.error("Error [{}] occurred while save op log [{}] ", e.getMessage(), resource);
    }
}
