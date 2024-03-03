/*
 *    Copyright (c) 2024.  lWoHvYe(Hongyan Wang)
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

import org.springframework.boot.gradle.tasks.bundling.BootBuildImage
import org.springframework.boot.gradle.tasks.run.BootRun

plugins {
    id("org.springframework.boot") // 通过引入这个，使该subModule有了相关的Task
    id("io.spring.dependency-management")
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.kotlin.spring)
    alias(libs.plugins.kotlin.jpa)
    alias(libs.plugins.kotlin.lombok)
}

val graalvmVersion = "23.1.0"

dependencies {
    implementation(project(":unicorn-security"))
    implementation(project(":unicorn-logging"))
    implementation("org.springframework.boot:spring-boot-starter-web") {
        exclude(group = "org.apache.tomcat.embed", module = "tomcat-embed-core")
        exclude(group = "org.apache.tomcat.embed", module = "tomcat-embed-websocket")
    }
    val tomcatVersion = dependencyManagement.importedProperties["tomcat.version"]
    implementation("org.apache.tomcat.experimental:tomcat-embed-programmatic:$tomcatVersion")
    implementation("org.apache.httpcomponents.client5:httpclient5")
    implementation(project(":unicorn-tp-tools-kotlin")) // kotlin as well
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    implementation("org.jetbrains.kotlin:kotlin-reflect")
    implementation(libs.bundles.coroutines)
    implementation("com.fasterxml.jackson.module:jackson-module-kotlin")
    testImplementation("org.jetbrains.kotlin:kotlin-test")
    testImplementation("org.springframework.boot:spring-boot-starter-test")
    // add following dependency if using ScriptEngine after Java 15
    implementation("org.graalvm.polyglot:js:$graalvmVersion")
    implementation("org.graalvm.js:js-scriptengine:$graalvmVersion")
    implementation(project(":unicorn-security")) {
        capabilities {
            requireCapability("com.lwohvye:unicorn-security-captcha")
        }
    }
    implementation(project(":unicorn-security")) {
        capabilities {
            // 这里只支撑横线，不支持驼峰
            requireCapability("com.lwohvye:unicorn-security-business-log")
        }
    }
}

tasks.jar {
    enabled = true
    manifest {
        attributes(
            "Automatic-Module-Name" to "lwohvye.${project.name.replace("-", ".")}"
        )
    }
}

tasks.withType<Test> {
    useJUnitPlatform()
    jvmArgs("--enable-preview")
}

tasks.withType<BootRun> {
    jvmArgs("-XX:+UseZGC", "--enable-preview")
}

tasks.withType<BootBuildImage> {
    environment.set(
        mapOf(
            "BP_JVM_TYPE" to "JDK",
            "BP_JVM_VERSION" to "21",
            "BPE_DELIM_JAVA_TOOL_OPTIONS" to " ",
            "BPE_APPEND_JAVA_TOOL_OPTIONS" to "-XX:+UseZGC --enable-preview"
        )
    )
}

kotlin {
    sourceSets.configureEach {
        languageSettings {
            apiVersion = "2.0"
        }
    }
    jvmToolchain(21)
}

