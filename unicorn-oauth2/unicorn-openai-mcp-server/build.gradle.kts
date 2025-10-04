plugins {
	id("org.springframework.boot")
	id("io.spring.dependency-management")
}

extra["springAiVersion"] = "1.0.2"

dependencies {
    implementation("org.springframework.ai:spring-ai-starter-mcp-server-webflux")
    testImplementation("org.springframework.boot:spring-boot-starter-test")
	testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

dependencyManagement {
	imports {
		mavenBom("org.springframework.ai:spring-ai-bom:${property("springAiVersion")}")
	}
}

