plugins {
    id("org.springframework.boot")
    id("io.spring.dependency-management")
}

dependencyManagement {
    imports {
        mavenBom("org.springframework.cloud:spring-cloud-dependencies:${property("springCloudVersion")}")
    }
}

dependencies {
    implementation("org.springframework.boot:spring-boot-starter-security")
    implementation("org.springframework.boot:spring-boot-starter-oauth2-client")
    implementation("org.springframework.cloud:spring-cloud-starter-gateway")
    implementation("org.springframework.boot:spring-boot-starter-thymeleaf")
    implementation("org.springframework.session:spring-session-data-redis")
    implementation(libs.springdoc.webflux.ui)
    implementation(libs.redisson)
    implementation("org.thymeleaf.extras:thymeleaf-extras-springsecurity6")
    implementation("org.webjars:webjars-locator-core")
    implementation("org.webjars:bootstrap:5.2.3")
    implementation("org.webjars:popper.js:2.9.3")
    implementation("org.webjars:jquery:3.6.4")

}
