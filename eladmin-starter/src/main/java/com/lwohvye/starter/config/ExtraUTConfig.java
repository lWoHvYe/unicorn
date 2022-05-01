/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package com.lwohvye.starter.config;

import com.lwohvye.sys.modules.system.enums.UserTypeEnum;
import com.lwohvye.utils.DynamicEnumHelper;
import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.List;
import java.util.Objects;

@Configuration
@ConfigurationProperties(prefix = "local.sys.extra-ut")
public class ExtraUTConfig {
    private static List<EUTo> list;

    public static List<EUTo> getList() {
        return list;
    }

    public void setList(List<EUTo> list) {
        ExtraUTConfig.list = list;
// TODO: 2022/5/1 除非把这部分下沉，否则想不到一个好的切入点来执行这个，当前想到的就是预留钩子给子类
        if (Objects.nonNull(list))
            refreshUserTypeEnum();
    }

    /**
     * 这里把额外配置几个类型加进去，这个至少要在callBack执行前进行才行
     *
     * @date 2022/5/1 1:22 PM
     */
    public void refreshUserTypeEnum() {
        for (var euTo : list) {
            DynamicEnumHelper.addEnum(UserTypeEnum.class, euTo.getName(), new Class[]{Integer.class, String.class}, new Object[]{euTo.getType(), euTo.getDesc()});
        }
    }

    @Getter
    @Setter
    public static class EUTo { // inner class may be static
        private String name;
        private Integer type;
        private String desc;
    }

}
