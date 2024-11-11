/*
 *    Copyright (c) 2023-2024.  lWoHvYe(Hongyan Wang)
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

/*
 * This file was generated by the Gradle 'init' task.
 */
import org.springframework.boot.gradle.plugin.SpringBootPlugin

plugins {
    id("me.champeau.mrjar")
}

description = "系统Core模块"

multiRelease {
    targetVersions(17, 21, 22)
}

java {
    withJavadocJar()
}

configurations {
    named("java21Implementation") {
        extendsFrom(implementation.get())
    }
    named("java22Implementation") {
        extendsFrom(implementation.get())
    }
}

val sharedManifest = rootProject.extra["sharedManifest"] as? Manifest

tasks.jar {
    enabled = true // separates boot jar from normal jar
    manifest {
        from(sharedManifest)
        attributes(
            "Implementation-Title" to project.name,
            "Implementation-Version" to project.version,
            "Automatic-Module-Name" to "lwohvye." + project.name.replace("-", ".")
        )
    }
    into("META-INF/maven/${project.group}/${project.name}") {
        from({ tasks["generatePomFileForMavenJavaCorePublication"] })
        rename(".*", "pom.xml")
    }
}

publishing {
    publications {
        create<MavenPublication>("mavenJavaCore") {
            from(components["java"])
            versionMapping {
                usage("java-api") {
                    fromResolutionOf("runtimeClasspath")
                }
                usage("java-runtime") {
                    fromResolutionResult()
                }
            }
            pom {
                name.set("Unicorn Core")
                description.set("Core module with Utils, QueryAnno and so on")
                url.set("https://github.com/lWoHvYe/unicorn.git")
                licenses {
                    license {
                        name.set("The Apache License, Version 2.0")
                        url.set("http://www.apache.org/licenses/LICENSE-2.0.txt")
                    }
                }
                developers {
                    developer {
                        id.set("lWoHvYe")
                        name.set("王红岩(lWoHvYe)")
                        email.set("lWoHvYe@outlook.com")
                        url.set("https://www.lwohvye.com")
                    }
                }
                scm {
                    connection.set("scm:git:git://github.com/lWoHvYe/unicorn.git")
                    developerConnection.set("scm:git:ssh://github.com/lWoHvYe/unicorn.git")
                    url.set("https://github.com/lWoHvYe/unicorn/tree/main")
                }
            }
        }
    }
}

dependencies {
    api(platform(SpringBootPlugin.BOM_COORDINATES))
    // java21TestImplementation("") // it is possible to add a dependency only used to compile the sources found in src/main/java21
    api("org.springframework.boot:spring-boot-starter-data-jpa")
    api("org.springframework.boot:spring-boot-starter-web")
    api("org.springframework.boot:spring-boot-starter-security")
    implementation("org.springframework.security:spring-security-oauth2-jose")
    api("org.springframework.boot:spring-boot-starter-amqp")
    api("org.springframework.boot:spring-boot-starter-cache")
    api("org.springframework.boot:spring-boot-starter-data-redis")
    api(libs.redisson)
    api("org.apache.commons:commons-pool2")
    api("org.apache.commons:commons-lang3")
    api(libs.springdoc.webmvc.ui)
    api(libs.hutool)
    api(libs.ip2region)
    api(libs.poi)
    api(libs.poi.ooxml)
    implementation(libs.xerces)
    api(libs.mapstruct)
//    mapstruct-spring-extensions seems unused
    api(libs.mapstruct.spring.annotations)
    api("org.hibernate.validator:hibernate-validator")
    api("com.github.ben-manes.caffeine:caffeine")
    implementation(libs.logback.encoder)
    api("org.bouncycastle:bcpkix-jdk18on:1.72")
    api(libs.thumbnailator)
    api("org.jetbrains:annotations:24.1.0")
    testImplementation("org.springframework.boot:spring-boot-starter-test")
    runtimeOnly("com.mysql:mysql-connector-j")
}

tasks.named("compileJava") {
    (this as JavaCompile).options.javaModuleVersion = project.version.toString()
}

tasks.javadoc {
    (this.options as StandardJavadocDocletOptions).addStringOption("-release", "17")
}
