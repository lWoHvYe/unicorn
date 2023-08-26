import org.springframework.boot.gradle.plugin.SpringBootPlugin

description = "Virtual Threads for Java 20"

tasks.withType<JavaCompile> {
    options.encoding = "UTF-8"
}

java {
    withJavadocJar()
}

tasks.jar {
    enabled = true // separates boot jar from normal jar
    manifest {
//        from(extra["sharedManifest"] as Manifest)
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
        from("generatePomFileForMavenVirtualThreadsPublication")
        rename(".*", "pom.xml")
    }
}

publishing {
    publications {
        create<MavenPublication>("mavenVirtualThreads") {
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
                name.set("Unicorn Virtual Threads")
                description.set("Virtual Threads for Java 20 + Spring Boot 3.2")
                url.set("https://github.com/WHY-lWoHvYe/valentine-p2p.git")

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
                    connection.set("scm:git:git://github.com/WHY-lWoHvYe/valentine-p2p.git")
                    developerConnection.set("scm:git:ssh://github.com/WHY-lWoHvYe/valentine-p2p.git")
                    url.set("https://github.com/WHY-lWoHvYe/valentine-p2p/tree/main")
                }
            }
        }
    }
}

dependencies {
    implementation(platform(SpringBootPlugin.BOM_COORDINATES))
    implementation("org.springframework.boot:spring-boot-starter")
}
