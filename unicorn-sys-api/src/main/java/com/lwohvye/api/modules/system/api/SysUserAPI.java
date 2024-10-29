/*
 *  Copyright 2019-2022 lWoHvYe
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
package com.lwohvye.api.modules.system.api;

import com.lwohvye.api.modules.system.service.dto.UserInnerDto;
import com.lwohvye.core.base.BaseEntity.Update;
import com.lwohvye.api.modules.system.domain.User;
import com.lwohvye.api.modules.system.domain.vo.UserBaseVo;
import com.lwohvye.api.modules.system.domain.vo.UserPassVo;
import com.lwohvye.api.modules.system.service.dto.UserQueryCriteria;
import com.lwohvye.core.utils.result.ResultInfo;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.service.annotation.*;

import java.util.Map;
import java.util.Set;

/**
 * @author Super idol lv
 * @website https://lwohvye.com
 * @date 2022-03-20
 **/
@HttpExchange(url = "/api/sys/users")
public interface SysUserAPI {
    // feign 中使用Method.getAnnotations()获取方法上注解，这个只能获取方法本身的，无法获取（被重写的）父类的方法上的（Returns annotations that are directly present on this element. This method ignores inherited annotations）
    // 另Spring提供了很强大的 AnnotatedElementUtils, 可以获取到父类方法上的注解
    // @Inherited可以实现类上的注解继承（方法上是不行的）
    // openFeign的逻辑不是很理解，外部Contract.parseAndValidateMetadata()通过Method.getAnnotations()获取方法上注解，
    // 然后调用SpringMvcContract.processAnnotationOnMethod()内部又通过AnnotatedElementUtils获取包含父类方法的注解，但子类没有相关注解就走不进去，子类如果有又会覆盖父类，那考虑父类的目的是什么

    // FeignClient不支持get方式传递实体类。通过把参数前的@RequestBody替换成@SpringQueryMap可以解决这个问题，但只支持单个实体类参数
    // 解决方案是引入 io.github.openfeign:feign-httpclient 并在配置中 feign.httpclient.enabled = true配置激活，之后即可配合@SpringQueryMap使用，且多个实体只需在第一个上添加。单个实体似乎是不用添加的
    @GetExchange
    Map<String, Object> query(UserQueryCriteria criteria, Pageable pageable);

    @PostExchange
    ResponseEntity<ResultInfo<String>> create(@Validated @RequestBody User resources);

    @PutExchange
    ResponseEntity<ResultInfo<String>> update(@Validated(Update.class) @RequestBody User resources) throws Exception;

    @PostExchange("/updateStatus")
    ResponseEntity<ResultInfo<String>> updateStatus(@RequestBody UserBaseVo userVo);

    @PutExchange("/center")
    ResponseEntity<ResultInfo<String>> center(@Validated(Update.class) @RequestBody User resources);

    @DeleteExchange
    ResultInfo<String> delete(@RequestBody Set<Long> ids);

    @PostExchange("/updatePass")
    ResultInfo<String> updatePass(@RequestBody UserPassVo passVo) throws Exception;

    @PostExchange("/updateAvatar")
    Map<String, String> updateAvatar(@RequestParam MultipartFile avatar);

    @PostExchange("/updateEmail/{code}")
    ResultInfo<String> updateEmail(@PathVariable String code, @RequestBody User user) throws Exception;

    @GetExchange("/name/{username}")
    UserInnerDto queryByName(@PathVariable String username);
}
