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
package com.lwohvye.api.modules.quartz.domain;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.persistence.*;
import lombok.Data;
import org.hibernate.annotations.CreationTimestamp;

import java.io.Serializable;
import java.sql.Timestamp;

/**
 * @author Zheng Jie
 * @date 2019-01-07
 */
@Entity
@Data
@Table(name = "sys_quartz_log")
public class QuartzLog implements Serializable {

    @Id
    @Column(name = "log_id")
    @Schema(description = "ID", accessMode = Schema.AccessMode.READ_ONLY)
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Schema(description = "任务名称", accessMode = Schema.AccessMode.READ_ONLY)
    private String jobName;

    @Schema(description = "bean名称", accessMode = Schema.AccessMode.READ_ONLY)
    private String beanName;

    @Schema(description = "方法名称", accessMode = Schema.AccessMode.READ_ONLY)
    private String methodName;

    @Schema(description = "参数", accessMode = Schema.AccessMode.READ_ONLY)
    private String params;

    @Schema(description = "cron表达式", accessMode = Schema.AccessMode.READ_ONLY)
    private String cronExpression;

    @Schema(description = "状态", accessMode = Schema.AccessMode.READ_ONLY)
    private Boolean isSuccess;

    @Schema(description = "异常详情", accessMode = Schema.AccessMode.READ_ONLY)
    private String exceptionDetail;

    @Schema(description = "执行耗时", accessMode = Schema.AccessMode.READ_ONLY)
    private Long time;

    @CreationTimestamp
    @Schema(description = "创建时间", accessMode = Schema.AccessMode.READ_ONLY)
    private Timestamp createTime;
}
