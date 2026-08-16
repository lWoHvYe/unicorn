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

import org.gradle.api.publish.maven.MavenPublication
import org.gradle.api.publish.maven.tasks.GenerateMavenPom
import org.gradle.jvm.tasks.Jar

plugins {
    `maven-publish`
    signing
}

publishing {
    repositories.maven {
        name = "GitHubPackages"
        url = uri("https://maven.pkg.github.com/lWoHvYe/unicorn")
        credentials {
            username = project.findProperty("gpr.user") as String? ?: System.getenv("USERNAME")
            password = project.findProperty("gpr.key") as String? ?: System.getenv("TOKEN")
        }
    }

    publications.withType<MavenPublication>().configureEach {
        versionMapping {
            usage("java-api") {
                fromResolutionOf("runtimeClasspath")
            }
            usage("java-runtime") {
                fromResolutionResult()
            }
        }

        pom {
            url.set("https://github.com/lWoHvYe/unicorn.git")
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
            }
        }

        if (project != rootProject) {
            val pomTask = tasks.named<GenerateMavenPom>(
                "generatePomFileFor${name.replaceFirstChar(Char::uppercaseChar)}Publication"
            )
            tasks.named<Jar>("jar") {
                into("META-INF/maven/${project.group}/${project.name}") {
                    from(pomTask)
                    rename(".*", "pom.xml")
                }
            }
        }
    }
}

signing {
    isRequired = !version.toString().endsWith("-SNAPSHOT") && System.getenv("CI") == null
    sign(publishing.publications)
}
