/*
 *    Copyright (c) 2023-2025.  lWoHvYe(Hongyan Wang)
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

// 这种是legacy的configuration，但针对未publish到maven central的，还是要这种方式才行(dependencies部分)
buildscript {
    repositories {
        gradlePluginPortal()
        // Should in hierarchy
        maven {
            name = "extra-repo"
            url = uri("$rootProject.projectDir/ex-lib")
        }
    }
}

plugins {
    id("com.lwohvye.java-conventions")
    //主要是定义了这个，定义了SpringBoot的Version相关，并提供了application, bootJar, bootBuildImage这些Task
    alias(libs.plugins.spring.boot) apply false
    // This plugin simplifies the use of Lombok in Gradle
    id("io.freefair.lombok") version "8.13"
    id("me.champeau.mrjar") version "0.1.1"
    id("org.gradlex.extra-java-module-info") version "1.11"
    // 在parent root执行Task，会同步执行sub project的Task，比如执行了bootJar，那会执行subPro的bootJar(若其中没有该Task会ignore)
    id("io.github.gradle-nexus.publish-plugin") version "2.0.0"
}

extra["sharedManifest"] = java.manifest {
    attributes(
        "Developer" to "lWoHvYe",
        "Created-By" to "Gradle",
        "Built-By" to System.getProperty("user.name"),
        "Build-Jdk-Spec" to System.getProperty("java.version")
    )
}

subprojects {
    apply(plugin = "com.lwohvye.java-conventions")
    apply(plugin = "org.gradlex.extra-java-module-info")
    apply(plugin = "io.freefair.lombok")

    // Setting a custom Lombok version when use plugin io.freefair.lombok
    lombok {
        version = "1.18.36"
    }

    dependencies {
        annotationProcessor("org.mapstruct", "mapstruct-processor", "1.6.2")
        annotationProcessor("org.mapstruct.extensions.spring", "mapstruct-spring-extensions", "1.1.2")
    }

    tasks.withType<Javadoc>().configureEach {
        isFailOnError = false
        (options as StandardJavadocDocletOptions).apply {
            addStringOption("Xdoclint:none", "-quiet")
            addStringOption("encoding", "UTF-8")
            addStringOption("charSet", "UTF-8")
            // Add support for custom tag
            addStringOption("tag", "date:a:Init Date:")
            addStringOption("tag", "author:a:Major Contributor:")
        }
    }

    extraJavaModuleInfo {
        // deriveAutomaticModuleNamesFromFileNames = true //  failed due to split package issue
        // legacy configure start
        failOnMissingModuleInfo.set(false)
        automaticModule("io.github.mouzt:bizlog-sdk", "bizlog.sdk")
        automaticModule("com.github.whvcse:easy-captcha", "easy.captcha")
        automaticModule("io.jsonwebtoken:jjwt-api", "jjwt.api")
        automaticModule("org.quartz-scheduler:quartz", "quartz")
        automaticModule("org.springframework.retry:spring-retry", "spring.retry")
        // end
    }
}

allprojects {
    tasks.withType<Javadoc> {
        if (JavaVersion.current().isJava9Compatible) {
            (options as StandardJavadocDocletOptions).addBooleanOption("html5", true)
        }
        if (JavaVersion.current().isCompatibleWith(JavaVersion.VERSION_21)) {
            (options as StandardJavadocDocletOptions).apply {
                addBooleanOption("-enable-preview", true)
                addStringOption("-release", "21")
            }
        }
    }
}

tasks.withType<Checkstyle>().configureEach {
    reports {
        xml.required.set(false)
    }
}


java {
    withJavadocJar()
    withSourcesJar()
}

publishing {
    publications {
        create<MavenPublication>("mavenJava") {
            artifactId = "unicorn"
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
                name.set("Valentine Unicorn")
                packaging = "pom"
                // optionally artifactId can be defined here
                description.set("A Spring Boot Project With Jpa JWT Security and so on")
                url.set("https://github.com/lWoHvYe/unicorn.git")
                properties = mapOf(
                    "myProp" to "chaste unicorn",
                    "spring-boot.version" to "${libs.versions.springBoot.get()}",
                    "project.core.version" to version
                )
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
                    tag.set("unicorn-v$version")
                }
            }
        }
    }
}

// this is the equivalent of the `nexusPublishing` block
nexusPublishing {
    repositories {
        sonatype {
            nexusUrl.set(uri("https://s01.oss.sonatype.org/service/local/"))
            snapshotRepositoryUrl.set(uri("https://s01.oss.sonatype.org/content/repositories/snapshots/"))
            username.set(findProperty("ossrhUsername") as String? ?: System.getenv("OSSRH_USERNAME"))
            password.set(findProperty("ossrhPassword") as String? ?: System.getenv("OSSRH_PASSWORD"))
        }
    }
}
