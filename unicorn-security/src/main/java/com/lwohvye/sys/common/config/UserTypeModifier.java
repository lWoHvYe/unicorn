/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.sys.common.config;

import com.lwohvye.sys.modules.system.enums.UserTypeEnum;
import com.lwohvye.core.utils.DynamicEnumHelper;
import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.List;
import java.util.Objects;

/**
 * 得益于之前的study,可以scan特定的properties并将其加入到UserTypeEnum中
 * 这样client不再局限于既有的几种userType，可以在extend strategy的同时，自由添加custom userType
 *
 * @date 2022/12/10 8:46 AM
 */
@Configuration
@ConfigurationProperties(prefix = "local.sys.extra-ut")
public class UserTypeModifier {
    private static List<UserType> userTypes;

    public static List<UserType> getUserTypes() {
        return userTypes;
    }

    public void setUserTypes(List<UserType> extraTypes) {
        UserTypeModifier.userTypes = extraTypes;
        if (Objects.nonNull(extraTypes))
            refreshUserTypeEnum();
    }

    /**
     * 这里把额外配置几个类型加进去，这个至少要在callBack执行前进行才行
     *
     * @date 2022/5/1 1:22 PM
     */
    void refreshUserTypeEnum() {
        for (var userType : userTypes) {
            DynamicEnumHelper.addEnum(UserTypeEnum.class, userType.getName(),
                    new Class[]{Integer.class, String.class}, new Object[]{userType.getType(), userType.getDesc()});
        }
    }

    @Getter
    @Setter
    static class UserType { // inner class may be static
        private String name;
        private Integer type;
        private String desc;
    }

}
