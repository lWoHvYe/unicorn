plugins {
    id("org.springframework.boot")
    id("io.spring.dependency-management")
}

extra["MyBatisPlusVersion"] = "3.5.12"

dependencies {
    implementation("org.springframework.boot:spring-boot-starter-web")
    implementation("com.baomidou:mybatis-plus-spring-boot3-starter:${property("MyBatisPlusVersion")}")
    implementation("com.baomidou:mybatis-plus-jsqlparser:${property("MyBatisPlusVersion")}")
    runtimeOnly("com.h2database:h2")
    testImplementation("org.springframework.boot:spring-boot-starter-test")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

tasks.bootJar {
    mainClass = "com.demo.MyBatisApplication"
}
