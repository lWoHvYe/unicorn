
description = "基础Bean模块"

java {
    withJavadocJar()
}

val sharedManifest = java.manifest {
    attributes(
        "Developer" to "lWoHvYe",
        "Created-By" to "Gradle",
        "Built-By" to System.getProperty("user.name"),
        "Build-Jdk-Spec" to System.getProperty("java.version"),
    )
}

tasks.jar {
    manifest {
        from(sharedManifest)
        attributes(
            "Implementation-Title" to project.name,
            "Implementation-Version" to project.version,
            "Automatic-Module-Name" to "lwohvye." + project.name.replace("-", ".")
        )
    }
    into("META-INF/maven/${project.group}/${project.name}") {
        from({ tasks["generatePomFileForMavenJavaBeansPublication"] })
        rename(".*", "pom.xml")
    }
}

publishing {
    publications {
        create<MavenPublication>("mavenJavaBeans") {
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
                name.set("Unicorn Beans")
                description.set("Beans & Configuration module")
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
    api(project(":unicorn-core"))
}
