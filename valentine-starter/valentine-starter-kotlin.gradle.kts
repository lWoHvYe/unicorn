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

import org.graalvm.buildtools.gradle.tasks.BuildNativeImageTask
import org.springframework.boot.gradle.tasks.aot.ProcessAot
import org.springframework.boot.gradle.tasks.bundling.BootBuildImage
import org.springframework.boot.gradle.tasks.run.BootRun

plugins {
    id("org.springframework.boot") // 通过引入这个，使该subModule有了相关的Task
    id("io.spring.dependency-management")
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.kotlin.spring)
    alias(libs.plugins.kotlin.jpa)
    alias(libs.plugins.kotlin.lombok)
    // Apply GraalVM Native Image plugin
//    id("org.graalvm.buildtools.native")
}

val graalvmEnable = false
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
    if (graalvmEnable) {
        environment.set(
            mapOf(
                "BP_JVM_VERSION" to "21.*",
                "BP_NATIVE_IMAGE" to "true",
                "BP_NATIVE_IMAGE_BUILD_ARGUMENTS" to "--enable-preview -J-Xmx7g " +
                        "-H:DeadlockWatchdogInterval=10 -H:+DeadlockWatchdogExitOnTimeout " +
                        "--initialize-at-build-time=ch.qos.logback.classic.Logger,ch.qos.logback.core.status.StatusBase" +
                        ",org.slf4j.LoggerFactory,ch.qos.logback.core.CoreConstants,ch.qos.logback.core.util.StatusPrinter" +
                        ",ch.qos.logback.core.status.InfoStatus,ch.qos.logback.classic.Level,ch.qos.logback.core.util.Loader " +
                        "-XX:+UseZGC -XX:+HeapDumpOnOutOfMemoryError --enable-preview",
                "BPE_DELIM_JAVA_TOOL_OPTIONS" to " ",
                "BPE_APPEND_JAVA_TOOL_OPTIONS" to "-XX:+UseZGC -XX:+HeapDumpOnOutOfMemoryError --enable-preview"
            )
        )
        buildpacks.set(
            listOf(
                "gcr.io/paketo-buildpacks/graalvm",
                "gcr.io/paketo-buildpacks/java-native-image"
            )
        )
    } else {
        environment.set(
            mapOf(
                "BP_JVM_TYPE" to "JDK",
                "BP_JVM_VERSION" to "21",
                "BPE_DELIM_JAVA_TOOL_OPTIONS" to " ",
                "BPE_APPEND_JAVA_TOOL_OPTIONS" to "-XX:+UseZGC --enable-preview"
            )
        )
    }
}

if (graalvmEnable) {
    tasks.withType<ProcessAot> {
        jvmArgs("--enable-preview")
    }

    tasks.withType<BuildNativeImageTask> {
        val options = options.get().asCompileOptions()
        options.buildArgs.addAll(
            "--initialize-at-build-time=ch.qos.logback.classic.Logger,org.slf4j.LoggerFactory" +
                    ",ch.qos.logback.core.status.StatusBase" +
                    ",ch.qos.logback.classic.Level,ch.qos.logback.core.CoreConstants,org.slf4j.MDC" +
                    ",ch.qos.logback.core.util.StatusPrinter,ch.qos.logback.core.util.Loader"
        )
        options.jvmArgs.addAll("--enable-preview")
    }
}

kotlin {
    sourceSets.configureEach {
        languageSettings {
            apiVersion = "2.0"
        }
    }
    jvmToolchain(21)
}

