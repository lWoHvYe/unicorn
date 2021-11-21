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
package com.lwohvye.modules.quartz.domain;

import com.lwohvye.base.BaseEntity;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @author Zheng Jie
 * @date 2019-01-07
 */
@Getter
@Setter
@Entity
@Table(name = "sys_quartz_job")
public class QuartzJob extends BaseEntity implements Serializable {

    public static final String JOB_KEY = "JOB_KEY";

    @Id
    @Column(name = "job_id")
    @NotNull(groups = {Update.class})
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Transient
    @Schema(description = "用于子任务唯一标识" , accessMode = Schema.AccessMode.READ_ONLY)
    private String uuid;

    @Schema(description = "定时器名称" )
    private String jobName;

    @NotBlank
    @Schema(description = "Bean名称" )
    private String beanName;

    @NotBlank
    @Schema(description = "方法名称" )
    private String methodName;

    @Schema(description = "参数" )
    private String params;

    @NotBlank
    @Schema(description = "cron表达式" )
    private String cronExpression;

    @Schema(description ="状态，暂时或启动")
    private Boolean isPause = false;

    @Schema(description = "负责人" )
    private String personInCharge;

    @Schema(description = "报警邮箱" )
    private String email;

    @Schema(description = "子任务" )
    private String subTask;

    @Schema(description = "失败后暂停" )
    private Boolean pauseAfterFailure;

    @NotBlank
    @Schema(description = "备注" )
    private String description;
}
