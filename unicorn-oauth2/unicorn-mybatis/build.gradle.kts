/*
 *    Copyright (c) 2026.  lWoHvYe(Hongyan Wang)
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

plugins {
    id("org.springframework.boot")
    id("io.spring.dependency-management")
}

extra["MyBatisPlusVersion"] = "3.5.12"

dependencies {
    implementation("org.springframework.boot:spring-boot-starter-webmvc")
    implementation("com.baomidou:mybatis-plus-spring-boot3-starter:${property("MyBatisPlusVersion")}")
    implementation("com.baomidou:mybatis-plus-jsqlparser:${property("MyBatisPlusVersion")}")
    runtimeOnly("com.h2database:h2")
    testImplementation("org.springframework.boot:spring-boot-starter-test")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

tasks.bootJar {
    mainClass = "com.demo.MyBatisApplication"
}
