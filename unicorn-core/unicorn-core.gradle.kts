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

import org.springframework.boot.gradle.plugin.SpringBootPlugin

plugins {
    id("me.champeau.mrjar")
}

description = "系统Core模块"

multiRelease {
    targetVersions(17, 21, 24, 25, 26)
}

configurations {
    named<Configuration>("java21Implementation") {
        extendsFrom(implementation.get())
    }
    named<Configuration>("java24Implementation") {
        extendsFrom(implementation.get())
    }
    named<Configuration>("java25Implementation") {
        extendsFrom(implementation.get())
    }
    named<Configuration>("java26Implementation") {
        extendsFrom(implementation.get())
    }
}

val sharedManifest = rootProject.extra["sharedManifest"] as? Manifest

tasks.jar {
    manifest {
        from(sharedManifest)
        attributes(
            "Implementation-Title" to project.name,
            "Implementation-Version" to project.version,
            "Automatic-Module-Name" to "lwohvye." + project.name.replace("-", ".")
        )
    }
}

publishing {
    publications {
        create<MavenPublication>("mavenJavaCore") {
            from(components["java"])
            pom {
                name.set("Unicorn Core")
                description.set("Core module with Utils, QueryAnno and so on")
            }
        }
    }
}

dependencies {
    implementation(platform(SpringBootPlugin.BOM_COORDINATES))

    // Spring Boot capabilities that form Unicorn Core's runtime surface.
    api("org.springframework.boot:spring-boot-starter-data-jpa")
    api("org.springframework.boot:spring-boot-starter-webmvc")
    api("org.springframework.boot:spring-boot-starter-security")
    implementation("org.springframework.boot:spring-boot-starter-restclient")
    implementation("org.springframework.security:spring-security-access")
    implementation("org.springframework.security:spring-security-oauth2-jose")
    api("org.springframework.boot:spring-boot-starter-amqp")
    api("org.springframework.boot:spring-boot-starter-cache")
    api("org.springframework.boot:spring-boot-starter-data-redis")
    api(libs.redisson)
    api(libs.redisson.cache)

    // Implementation details: these are used by Core itself but are not part of
    // the public type surface and should not leak through published API metadata.
    implementation("org.apache.commons:commons-pool2")
    implementation("org.apache.commons:commons-lang3")
    implementation(libs.springdoc.webmvc.ui)
    implementation(libs.hutool)
    implementation(libs.ip2region)
    implementation(libs.poi)
    implementation(libs.poi.ooxml)
    implementation(libs.xerces)
    api(libs.mapstruct)
    api(libs.mapstruct.spring.annotations)
    implementation("com.github.ben-manes.caffeine:caffeine")
    implementation(libs.logback.encoder)
    implementation(libs.bouncycastle.pkix)
    implementation(libs.thumbnailator)
    api(libs.jetbrains.annotations)
    api("org.springframework.boot:spring-boot-starter-actuator")
    api("io.micrometer:micrometer-tracing-bridge-brave")
    implementation("io.projectreactor.netty:reactor-netty-http")
    testImplementation("org.springframework.boot:spring-boot-starter-test")
    runtimeOnly("com.mysql:mysql-connector-j")
}

tasks.named<JavaCompile>("compileJava") {
    options.javaModuleVersion = project.version.toString()
}

tasks.named<Javadoc>("javadoc") {
    (options as StandardJavadocDocletOptions).addStringOption("-release", "17")
}
