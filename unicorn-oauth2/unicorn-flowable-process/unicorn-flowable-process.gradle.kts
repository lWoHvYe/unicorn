plugins {
	id("org.springframework.boot")
	id("io.spring.dependency-management")
}


dependencies {
	implementation("org.springframework.boot:spring-boot-starter")
	implementation("org.flowable:flowable-spring-boot-starter-process-rest:7.1.0")
	implementation("com.h2database:h2")
	testImplementation("org.springframework.boot:spring-boot-starter-test")
	testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

tasks.withType<Test> {
	useJUnitPlatform()
}
