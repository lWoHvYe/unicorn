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

package com.lwohvye.starter.modules.rest;

import com.lwohvye.starter.modules.service.ITSService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/anonymous/tsScope")
@RequiredArgsConstructor
public class TSController {
    @Autowired
    private ITSService ITSService;


    private String name;


    @GetMapping(value = "/{username}")
    public void userProfile(@PathVariable("username") String username) {
        name = username;

        ITSService.setField(name);

        try {
            for (int i = 0; i < 10; i++) {
                System.out.println(
                        Thread.currentThread().getId()
                        + "name:" + name
                        + "--ts:"
                        + ITSService.outIn());
                Thread.sleep(2000);
            }
        } catch (InterruptedException ignored) {
        }
    }

}
