/*
 *    Copyright (c) 2025.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.unicornopenai.tools;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.context.i18n.LocaleContextHolder;

public class DateTimeWeatherTools {

    @Tool(description = "Get the current date and time in the user's timezone")
    String getCurrentDateTime(ToolContext toolContext) {
        System.out.println("tenantId -> " + toolContext.getContext().get("tenantId"));
        return LocalDateTime.now().atZone(LocaleContextHolder.getTimeZone().toZoneId()).toString();
    }

    @Tool(description = "Set a user alarm for the given time, provided in ISO-8601 format")
    void setAlarm(String time) {
        LocalDateTime alarmTime = LocalDateTime.parse(time, DateTimeFormatter.ISO_DATE_TIME);
        System.out.println("Alarm set for " + alarmTime);
    }

    @Tool(description = "Get weather for a given city with given dateTime")
    String getWeatherByCity(String city, String dateTime) {
        System.out.println("Get weather for " + city + " and " + dateTime);
        return "It's always sunny in " + city + " at " + dateTime;
    }
}
