plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.kotlin.spring)
    alias(libs.plugins.kotlin.jpa)
    alias(libs.plugins.kotlin.lombok)
}

description = "3rd工具模块"

tasks.withType<JavaCompile> {
    options.encoding = "UTF-8"
}

java {
    withJavadocJar()
}

tasks.jar {
    enabled = true // separates boot jar from normal jar
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
        from("generatePomFileForMavenJava3rdToolPublication")
        rename(".*", "pom.xml")
    }
}

publishing {
    publications {
        create<MavenPublication>("mavenJava3rdTool") {
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
                name.set("Unicorn 3rd Tools")
                description.set("Tools and Third Party Integration")
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
    api(project(":unicorn-core"))
    api("org.springframework.boot:spring-boot-starter-mail")
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    implementation("org.jetbrains.kotlin:kotlin-reflect")
    implementation(libs.bundles.coroutines)
    implementation("com.fasterxml.jackson.module:jackson-module-kotlin")
    implementation("org.springframework.boot:spring-boot-starter-freemarker")
    implementation("software.amazon.awssdk:s3-transfer-manager:2.18.28-PREVIEW")
    testImplementation("org.jetbrains.kotlin:kotlin-test")
}

kotlin {
    sourceSets.configureEach {
        languageSettings {
            apiVersion = "2.0"
        }
    }
    jvmToolchain(21)
}
