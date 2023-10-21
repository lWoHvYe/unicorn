plugins {
    id("org.springframework.boot")
    id("io.spring.dependency-management")
}

tasks.jar {
    enabled = true
    manifest {
        attributes(
            "Automatic-Module-Name" to "lwohvye.${project.name.replace("-", ".")}"
        )
    }
}

dependencies {
    implementation(project(":unicorn-beans"))
    implementation("org.springframework.boot:spring-boot-starter-oauth2-resource-server")
    implementation("org.springframework.boot:spring-boot-starter-security")
}
