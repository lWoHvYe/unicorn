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
    alias(libs.plugins.spring.boot) apply false
    id("io.freefair.lombok") version "9.5.0"
    id("me.champeau.mrjar") version "0.1.1"
    id("org.gradlex.extra-java-module-info") version "1.14.2"
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

    lombok {
        version = "1.18.46"
    }

    dependencies {
        annotationProcessor("org.mapstruct", "mapstruct-processor", "1.6.3")
        annotationProcessor("org.mapstruct.extensions.spring", "mapstruct-spring-extensions", "2.0.0")
    }

    tasks.withType<Javadoc>().configureEach {
        isFailOnError = false
        (options as StandardJavadocDocletOptions).apply {
            encoding = "UTF-8"
            charSet = "UTF-8"
            docEncoding = "UTF-8"
            addBooleanOption("Xdoclint:none", true)
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

allprojects {
    tasks.withType<Javadoc>().configureEach {
        if (JavaVersion.current().isCompatibleWith(JavaVersion.VERSION_25)) {
            (options as StandardJavadocDocletOptions).addStringOption("-release", "25")
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
                description.set("A Spring Boot Project With Jpa JWT Security and so on")
                url.set("https://github.com/lWoHvYe/unicorn.git")
                properties = mapOf(
                    "myProp" to "chaste unicorn",
                    "spring-boot.version" to libs.versions.springBoot.get(),
                    "project.core.version" to version
                )
                licenses {
                    license {
                        name.set("The Apache License, Version 2.0")
                        url.set("https://www.apache.org/licenses/LICENSE-2.0.txt")
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

nexusPublishing {
    repositories {
        sonatype {
            nexusUrl.set(uri("https://ossrh-staging-api.central.sonatype.com/service/local/"))
            snapshotRepositoryUrl.set(uri("https://central.sonatype.com/repository/maven-snapshots/"))
            username.set(findProperty("ossrhUsername") as String? ?: System.getenv("OSSRH_USERNAME"))
            password.set(findProperty("ossrhPassword") as String? ?: System.getenv("OSSRH_PASSWORD"))
        }
    }
}
