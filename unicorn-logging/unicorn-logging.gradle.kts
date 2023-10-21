/*
 * This file was generated by the Gradle 'init' task.
 */

description = "日志收集模块"

java {
    withJavadocJar()
}

tasks.withType<JavaCompile> {
    options.encoding = "UTF-8"
}

tasks.jar {
    enabled = true
    manifest {
        attributes(
            mapOf(
                "Developer" to "lWoHvYe",
                "Created-By" to "Gradle",
                "Built-By" to System.getProperty("user.name"),
                "Build-Jdk-Spec" to System.getProperty("java.version"),
                "Implementation-Title" to project.name,
                "Implementation-Version" to project.version,
                "Automatic-Module-Name" to "lwohvye.${project.name.replace("-", ".")}"
            )
        )
    }
    into("META-INF/maven/${project.group}/${project.name}") {
        from("generatePomFileForMavenJavaLogPublication")
        rename(".*", "pom.xml")
    }
}

publishing {
    publications {
        create<MavenPublication>("mavenJavaLog") {
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
                name.set("Unicorn Log Collector")
                description.set("Logging module with API for store and retrieve")
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
    api(project(":unicorn-beans"))
}
