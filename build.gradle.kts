/*
 *    Copyright (c) 2023-2026.  lWoHvYe(Hongyan Wang)
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
    id("com.lwohvye.java-conventions")
    id("com.lwohvye.publishing-conventions")
    alias(libs.plugins.spring.boot) apply false
    alias(libs.plugins.freefair.lombok)
    alias(libs.plugins.mrjar)
    alias(libs.plugins.extra.java.module.info)
    alias(libs.plugins.nexus.publish)
}

extra["sharedManifest"] = java.manifest {
    attributes(
        "Developer" to "lWoHvYe",
        "Created-By" to "Gradle",
        "Built-By" to System.getProperty("user.name"),
        "Build-Jdk-Spec" to System.getProperty("java.version")
    )
}

val lombokVersion = libs.versions.lombok.get()
val mapstructProcessor = libs.mapstruct.processor
val mapstructSpring = libs.mapstruct.spring

subprojects {
    apply(plugin = "com.lwohvye.java-conventions")
    apply(plugin = "com.lwohvye.publishing-conventions")
    apply(plugin = "org.gradlex.extra-java-module-info")
    apply(plugin = "io.freefair.lombok")

    lombok {
        version = lombokVersion
    }

    dependencies {
        annotationProcessor(mapstructProcessor)
        annotationProcessor(mapstructSpring)
    }

    tasks.withType<Javadoc>().configureEach {
        isFailOnError = false
        (options as StandardJavadocDocletOptions).apply {
            encoding = "UTF-8"
            charSet = "UTF-8"
            docEncoding = "UTF-8"
            addBooleanOption("Xdoclint:none", true)
            addBooleanOption("enable-preview", true)
            addStringOption("tag", "date:a:Init Date:")
            addStringOption("tag", "author:a:Major Contributor:")
        }
    }

    extraJavaModuleInfo {
        failOnMissingModuleInfo.set(false)
        automaticModule("org.springframework.security:spring-security-core", "spring.security.core") {
            mergeJar("org.springframework.security:spring-security-web")
            mergeJar("org.springframework.security:spring-security-access")
        }
        automaticModule("io.github.mouzt:bizlog-sdk", "bizlog.sdk")
        automaticModule("com.github.whvcse:easy-captcha", "easy.captcha")
        automaticModule("io.jsonwebtoken:jjwt-api", "jjwt.api")
    }
}

tasks.withType<Checkstyle>().configureEach {
    reports {
        xml.required.set(false)
    }
}

publishing {
    publications {
        create<MavenPublication>("mavenJava") {
            artifactId = "unicorn"
            from(components["java"])
            pom {
                name.set("Valentine Unicorn")
                description.set("A Spring Boot Project With Jpa JWT Security and so on")
                properties = mapOf(
                    "myProp" to "chaste unicorn",
                    "spring-boot.version" to libs.versions.springBoot.get(),
                    "project.core.version" to version
                )
                scm {
                    tag.set("unicorn-v$version")
                }
            }
        }
    }
}

nexusPublishing {
    repositories {
        sonatype {
            nexusUrl.set(uri("https://ossrh-staging-api.central.sonatype.com/service/local/"))
            snapshotRepositoryUrl.set(uri("https://central.sonatype.com/repository/maven-snapshots/"))
            username.set(
                providers.gradleProperty("ossrhUsername")
                    .orElse(providers.environmentVariable("OSSRH_USERNAME"))
            )
            password.set(
                providers.gradleProperty("ossrhPassword")
                    .orElse(providers.environmentVariable("OSSRH_PASSWORD"))
            )
        }
    }
}