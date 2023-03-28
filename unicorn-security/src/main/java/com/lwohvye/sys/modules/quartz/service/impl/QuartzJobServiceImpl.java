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
package com.lwohvye.sys.modules.quartz.service.impl;

import cn.hutool.core.util.IdUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import com.lwohvye.sys.modules.quartz.domain.QuartzJob;
import com.lwohvye.sys.modules.quartz.domain.QuartzLog;
import com.lwohvye.sys.modules.quartz.service.dto.JobQueryCriteria;
import com.lwohvye.core.exception.BadRequestException;
import com.lwohvye.core.utils.*;
import com.lwohvye.core.utils.redis.RedisUtils;
import com.lwohvye.sys.modules.quartz.repository.QuartzJobRepository;
import com.lwohvye.sys.modules.quartz.repository.QuartzLogRepository;
import com.lwohvye.sys.modules.quartz.service.IQuartzJobService;
import com.lwohvye.sys.modules.quartz.utils.QuartzManage;
import lombok.RequiredArgsConstructor;
import org.quartz.CronExpression;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.*;

/**
 * @author Zheng Jie
 * @date 2019-01-07
 */
@RequiredArgsConstructor
@Service(value = "quartzJobService")
public class QuartzJobServiceImpl implements IQuartzJobService {

    private final QuartzJobRepository quartzJobRepository;
    private final QuartzLogRepository quartzLogRepository;
    private final QuartzManage quartzManage;
    private final RedisUtils redisUtils;

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public Map<String, Object> queryAll(JobQueryCriteria criteria, Pageable pageable) {
        return PageUtils.toPage(quartzJobRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), pageable));
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public Map<String, Object> queryAllLog(JobQueryCriteria criteria, Pageable pageable) {
        return PageUtils.toPage(quartzLogRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder), pageable));
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public List<QuartzJob> queryAll(JobQueryCriteria criteria) {
        return quartzJobRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder));
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public List<QuartzLog> queryAllLog(JobQueryCriteria criteria) {
        return quartzLogRepository.findAll((root, criteriaQuery, criteriaBuilder) -> QueryHelp.getPredicate(root, criteria, criteriaBuilder));
    }

    @Override
    @Transactional(rollbackFor = Exception.class, readOnly = true)
    public QuartzJob findById(Long id) {
        QuartzJob quartzJob = quartzJobRepository.findById(id).orElseGet(QuartzJob::new);
        ValidationUtils.isNull(quartzJob.getId(), "QuartzJob", "id", id);
        return quartzJob;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void create(QuartzJob resources) {
        if (!CronExpression.isValidExpression(resources.getCronExpression())) {
            throw new BadRequestException("cron表达式格式错误");
        }
        resources = quartzJobRepository.save(resources);
        quartzManage.addJob(resources);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(QuartzJob resources) {
        if (!CronExpression.isValidExpression(resources.getCronExpression())) {
            throw new BadRequestException("cron表达式格式错误");
        }
        if (StringUtils.isNotBlank(resources.getSubTask())) {
            List<String> tasks = Arrays.asList(resources.getSubTask().split("[,，]"));
            if (tasks.contains(resources.getId().toString())) {
                throw new BadRequestException("子任务中不能添加当前任务ID");
            }
        }
        resources = quartzJobRepository.save(resources);
        quartzManage.updateJobCron(resources);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateIsPause(QuartzJob quartzJob) {
        if (Boolean.TRUE.equals(quartzJob.getIsPause())) {
            quartzManage.resumeJob(quartzJob);
            quartzJob.setIsPause(false);
        } else {
            quartzManage.pauseJob(quartzJob);
            quartzJob.setIsPause(true);
        }
        quartzJobRepository.save(quartzJob);
    }

    @Override
    public void execution(QuartzJob quartzJob) {
        quartzManage.runJobNow(quartzJob);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Set<Long> ids) {
        for (Long id : ids) {
            QuartzJob quartzJob = findById(id);
            quartzManage.deleteJob(quartzJob);
            quartzJobRepository.delete(quartzJob);
        }
    }

    @Override
    public void saveLog(QuartzLog quartzLog) {
        quartzLogRepository.save(quartzLog);
    }

    @Async
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void executionSubJob(String[] tasks) throws InterruptedException {
        for (String id : tasks) {
            if (StrUtil.isBlank(id)) {
                // 如果是手动清除子任务id，会出现id为空字符串的问题
                continue;
            }
            QuartzJob quartzJob = findById(Long.parseLong(id));
            // 执行任务
            String uuid = IdUtil.simpleUUID();
            quartzJob.setUuid(uuid);
            // 执行任务
            execution(quartzJob);
            // 获取执行状态，如果执行失败则停止后面的子任务执行
            Boolean result = (Boolean) redisUtils.get(uuid);
            while (ObjectUtil.isNull(result)) {
                // 休眠5秒，再次获取子任务执行情况
                Thread.sleep(5000);
                result = (Boolean) redisUtils.get(uuid);
            }
            if (Boolean.FALSE.equals(result)) {
                redisUtils.delete(uuid);
                break;
            }
        }
    }

    @Override
    public void download(List<QuartzJob> quartzJobs, HttpServletResponse response) throws IOException {
        List<Map<String, Object>> list = new ArrayList<>();
        for (QuartzJob quartzJob : quartzJobs) {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("任务名称", quartzJob.getJobName());
            map.put("Bean名称", quartzJob.getBeanName());
            map.put("执行方法", quartzJob.getMethodName());
            map.put("参数", quartzJob.getParams());
            map.put("表达式", quartzJob.getCronExpression());
            map.put("状态", quartzJob.getIsPause() ? "暂停中" : "运行中");
            map.put("描述", quartzJob.getDescription());
            map.put("创建日期", quartzJob.getCreateTime());
            list.add(map);
        }
        FileUtils.downloadExcel(list, response);
    }

    @Override
    public void downloadLog(List<QuartzLog> queryAllLog, HttpServletResponse response) throws IOException {
        List<Map<String, Object>> list = new ArrayList<>();
        for (QuartzLog quartzLog : queryAllLog) {
            Map<String, Object> map = new LinkedHashMap<>();
            map.put("任务名称", quartzLog.getJobName());
            map.put("Bean名称", quartzLog.getBeanName());
            map.put("执行方法", quartzLog.getMethodName());
            map.put("参数", quartzLog.getParams());
            map.put("表达式", quartzLog.getCronExpression());
            map.put("异常详情", quartzLog.getExceptionDetail());
            map.put("耗时/毫秒", quartzLog.getTime());
            map.put("状态", quartzLog.getIsSuccess() ? "成功" : "失败");
            map.put("创建日期", quartzLog.getCreateTime());
            list.add(map);
        }
        FileUtils.downloadExcel(list, response);
    }
}
