/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.core.annotation;

import org.springframework.web.bind.annotation.ResponseBody;

import java.lang.annotation.*;

/**
 * 通用返回注解
 * 当ReturnType = ResponseEntity 且 HttpStatus = OK 时，改为 T + @RespResultBody
 * 当 HttpStatus = OK 时，可略去ResponseEntity这一层
 * 当 HttpStatus != OK 时，使用@RespResultBody可略去body中对ResultInfo.success(T) 的调用，只保留T
 *
 * @author Hongyan Wang
 * @date 2021/1/15 19:44
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Documented
@ResponseBody
public @interface RespResultBody {

}
