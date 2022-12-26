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
package com.lwohvye.sys.modules.quartz.utils;

import cn.hutool.core.util.StrUtil;
import com.lwohvye.api.modules.quartz.domain.QuartzJob;
import com.lwohvye.api.modules.quartz.domain.QuartzLog;
import com.lwohvye.core.config.ValentineExecutorConfig;
import com.lwohvye.core.utils.MailAdapter;
import com.lwohvye.core.utils.SpringContextHolder;
import com.lwohvye.core.utils.StringUtils;
import com.lwohvye.core.utils.ThrowableUtils;
import com.lwohvye.core.utils.redis.RedisUtils;
import com.lwohvye.sys.modules.quartz.service.IQuartzJobService;
import lombok.extern.slf4j.Slf4j;
import org.quartz.JobExecutionContext;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.quartz.QuartzJobBean;

import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * 参考人人开源，https://gitee.com/renrenio/renren-security
 *
 * @author /
 * @date 2019-01-07
 * @see ValentineExecutorConfig asyncTaskExecutor()，use this pool for JobRunning
 */
@Async
@Slf4j
public class ExecutionJob extends QuartzJobBean {

    /**
     * 该处仅供参考
     */
    private static final ExecutorService VIRTUAL_EXECUTOR =
            Executors.newThreadPerTaskExecutor(Thread.ofVirtual().name("Virtual-Job").factory());

    @Override
    public void executeInternal(JobExecutionContext context) {
        QuartzJob quartzJob = (QuartzJob) context.getMergedJobDataMap().get(QuartzJob.JOB_KEY);
        // 获取spring bean
        IQuartzJobService quartzJobService = SpringContextHolder.getBean(IQuartzJobService.class);
        RedisUtils redisUtils = SpringContextHolder.getBean(RedisUtils.class);

        String uuid = quartzJob.getUuid();

        QuartzLog quartzLog = new QuartzLog();
        quartzLog.setJobName(quartzJob.getJobName());
        quartzLog.setBeanName(quartzJob.getBeanName());
        quartzLog.setMethodName(quartzJob.getMethodName());
        quartzLog.setParams(quartzJob.getParams());
        var startTime = Instant.now();
        quartzLog.setCronExpression(quartzJob.getCronExpression());
        try {
            // 执行任务
            QuartzRunnable task = new QuartzRunnable(quartzJob.getBeanName(), quartzJob.getMethodName(), quartzJob.getParams());
            Future<?> future = VIRTUAL_EXECUTOR.submit(task); // 这里好像没什么用途（只是统计下执行用时的样子），因为标记了Async，这里由async-pool执行
            future.get();
            long times = Duration.between(startTime, Instant.now()).toMillis();
            quartzLog.setTime(times);
            if (StringUtils.isNotBlank(uuid)) {
                redisUtils.set(uuid, true);
            }
            // 任务状态
            quartzLog.setIsSuccess(true);
            // 判断是否存在子任务
            if (StrUtil.isNotBlank(quartzJob.getSubTask())) {
                String[] tasks = quartzJob.getSubTask().split("[,，]");
                // 执行子任务
                quartzJobService.executionSubJob(tasks);
            }
        } catch (Exception e) {
            if (StringUtils.isNotBlank(uuid)) {
                redisUtils.set(uuid, false);
            }
            long times = Duration.between(startTime, Instant.now()).toMillis();
            quartzLog.setTime(times);
            // 任务状态 0：成功 1：失败
            quartzLog.setIsSuccess(false);
            quartzLog.setExceptionDetail(ThrowableUtils.getStackTrace(e));
            // 任务如果失败了则暂停
            if (quartzJob.getPauseAfterFailure() != null && quartzJob.getPauseAfterFailure()) {
                quartzJob.setIsPause(false);
                //更新状态
                quartzJobService.updateIsPause(quartzJob);
            }
            // 邮箱报警
            if (StrUtil.isNotBlank(quartzJob.getEmail())) {
                var to = quartzJob.getEmail();
                var subject = "定时任务【" + quartzJob.getJobName() + "】执行失败，请尽快处理！";
                var templateName = "email/taskAlarm.ftl";
                Map<String, Object> paramsMap = Map.of("task", quartzJob, "msg", ThrowableUtils.getStackTrace(e)); // 这里用var类型推断，会是Map<String, Serializable>
                var res = MailAdapter.sendTemplatedMail(to, subject, templateName, paramsMap);
                log.error("Task Error，Name {} || Reason {} || NoticeRes {} ", quartzJob.getJobName(), e.getMessage(), res);
            }
//            执行失败再记录日志
            quartzJobService.saveLog(quartzLog);
        }
    }
}
