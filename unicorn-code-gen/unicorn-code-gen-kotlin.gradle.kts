plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.kotlin.spring)
    alias(libs.plugins.kotlin.jpa)
    alias(libs.plugins.kotlin.lombok)
}

description = "代码生成模块"

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
        from("generatePomFileForMavenJavaCodeGenPublication")
        rename(".*", "pom.xml")
    }
}

publishing {
    publications {
        create<MavenPublication>("mavenJavaCodeGen") {
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
                name.set("Unicorn Code Generator")
                description.set("Server and Web Code Generator")
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
    api("org.springframework.boot:spring-boot-starter-freemarker")
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    implementation("org.jetbrains.kotlin:kotlin-reflect")
    implementation(libs.bundles.coroutines)
    implementation("com.fasterxml.jackson.module:jackson-module-kotlin")
    implementation(libs.commons.configuration)
    implementation(libs.commons.beanutils)
    testImplementation("org.jetbrains.kotlin:kotlin-test")
}

kotlin {
    sourceSets.configureEach {
        languageSettings {
            apiVersion = "2.0"
        }
    }
    jvmToolchain(20)
}
