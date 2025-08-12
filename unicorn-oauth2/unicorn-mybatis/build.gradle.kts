plugins {
	id("org.springframework.boot")
	id("io.spring.dependency-management")
}

dependencies {
	implementation("org.springframework.boot:spring-boot-starter-web")
    implementation("com.baomidou:mybatis-plus-spring-boot3-starter:3.5.12")
    implementation("com.baomidou:mybatis-plus-jsqlparser:3.5.12")
    runtimeOnly("com.h2database:h2")
	testImplementation("org.springframework.boot:spring-boot-starter-test")
	testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}


