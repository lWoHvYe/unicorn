/*
 *    Copyright (c) 2022-2023.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.search.config;

import org.springframework.beans.propertyeditors.StringTrimmerEditor;
import org.springframework.format.datetime.DateFormatter;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.InitBinder;

/*
Bind request parameters (that is, form or query data) to a model object. //通过json传是不行的
Convert String-based request values (such as request parameters, path variables, headers, cookies, and others) to the target type of controller method arguments.
Format model object values as String values when rendering HTML forms.Bind request parameters (that is, form or query data) to a model object.
Convert String-based request values (such as request parameters, path variables, headers, cookies, and others) to the target type of controller method arguments.
Format model object values as String values when rendering HTML forms.
 */
@ControllerAdvice
@Controller
public class LocalWebBindingInitializer {

    /*
    忽略前端传的空字符串，使用@ControllerAdvice应该全局生效，每次请求都会走进来
     */
    @InitBinder
    public void ignEmpStrInitBinder(WebDataBinder binder) {
        binder.registerCustomEditor(String.class, new StringTrimmerEditor(true));
    }

    // @InitBinder
    // public void dateFormatInitBinder(WebDataBinder binder) {
    //     SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
    //     dateFormat.setLenient(false);
    //     binder.registerCustomEditor(Date.class, new CustomDateEditor(dateFormat, false));
    // }

    /*
    Alternatively, when you use a Formatter-based setup through a shared FormattingConversionService,
    you can re-use the same approach and register controller-specific Formatter implementations, as the following example shows:
    复用原来的可以用下面的方式
     */
    @InitBinder
    public void dateFormatInitBinder(WebDataBinder binder) {
        binder.addCustomFormatter(new DateFormatter("dd-MMM-yy"));
    }
}
