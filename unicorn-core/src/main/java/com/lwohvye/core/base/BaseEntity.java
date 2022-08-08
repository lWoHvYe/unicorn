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
package com.lwohvye.core.base;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.Column;
import javax.persistence.EntityListeners;
import javax.persistence.MappedSuperclass;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.sql.Timestamp;

/**
 * 通用字段， is_del 根据需求自行添加
 *
 * @author Zheng Jie
 * @Date 2019年10月24日20:46:32
 */
@Getter
@Setter
@MappedSuperclass // @MappedSuperclass 用在父类上面。当这个类肯定是父类时，加此标注。如果改成@Entity，则继承后，多个类继承，只会生成一个表，而不是多个继承，生成多个表
@EntityListeners(AuditingEntityListener.class)
public class BaseEntity implements Serializable {

    @CreatedBy
    @Column(name = "create_by", updatable = false)
    @Schema(description = "创建人", accessMode = Schema.AccessMode.READ_ONLY)
    private String createBy;

    @LastModifiedBy
    @Column(name = "update_by")
    @Schema(description = "更新人", accessMode = Schema.AccessMode.READ_ONLY)
    private String updateBy;

    @CreationTimestamp
    @Column(name = "create_time", updatable = false)
    @Schema(description = "创建时间", accessMode = Schema.AccessMode.READ_ONLY)
    private Timestamp createTime;

    @UpdateTimestamp
    @Column(name = "update_time")
    @Schema(description = "更新时间", accessMode = Schema.AccessMode.READ_ONLY)
    private Timestamp updateTime;

/*
    // 乐观锁Version
    private Integer version;

    // 逻辑删除 deleted=0 ==>deleted =1(失效)
    private Integer deleted;
*/

    /* 分组校验 */
    public @interface Create {
    }

    /* 分组校验 */
    public @interface Update {
    }

    @Override
    public String toString() { // 通用的ToString方法
        var builder = new ToStringBuilder(this);
        var fields = this.getClass().getDeclaredFields(); // 这里只获取本类的全部域
        try {
            for (var f : fields) {
                // f.setAccessible(true); 用下面的方式更好一些
                if (f.trySetAccessible())
                    builder.append(f.getName(), f.get(this)).append("\n");
            }
        } catch (Exception e) {
            builder.append("toString builder encounter an error");
        }
        return builder.toString();
    }
}
