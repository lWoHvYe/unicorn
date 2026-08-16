/*
 *    Copyright (c) 2024-2026.  lWoHvYe(Hongyan Wang)
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
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.kotlin.spring)
    alias(libs.plugins.kotlin.jpa)
    alias(libs.plugins.kotlin.lombok)
}

description = "代码生成模块"

val sharedManifest = rootProject.extra["sharedManifest"] as? Manifest

tasks.jar {
    manifest {
        from(sharedManifest)
        attributes(
            "Implementation-Title" to project.name,
            "Implementation-Version" to project.version,
            "Automatic-Module-Name" to "lwohvye.${project.name.replace("-", ".")}"
        )
    }
}

publishing {
    publications {
        create<MavenPublication>("mavenJavaCodeGen") {
            from(components["java"])
            pom {
                name.set("Unicorn Code Generator")
                description.set("Server and Web Code Generator")
            }
        }
    }
}

dependencies {
    api(project(":unicorn-beans"))
    api("org.springframework.boot:spring-boot-starter-freemarker")
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    implementation("org.jetbrains.kotlin:kotlin-reflect")
    implementation(libs.bundles.coroutines)
    implementation("com.fasterxml.jackson.module:jackson-module-kotlin")
    implementation(libs.commons.configuration)
    implementation(libs.commons.beanutils)
    testImplementation("org.jetbrains.kotlin:kotlin-test")
}

kotlin {
    sourceSets.configureEach {
        languageSettings {
            apiVersion = "2.0"
        }
    }
}
