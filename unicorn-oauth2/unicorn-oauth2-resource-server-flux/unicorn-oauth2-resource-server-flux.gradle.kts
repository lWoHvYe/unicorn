import org.springframework.boot.gradle.plugin.SpringBootPlugin

dependencies {
    implementation(platform(SpringBootPlugin.BOM_COORDINATES))
    implementation("org.springframework.boot:spring-boot-starter-oauth2-resource-server")
    implementation("org.springframework.boot:spring-boot-starter-webflux")
    implementation("org.springframework.boot:spring-boot-starter-security")
    implementation("org.springframework.boot:spring-boot-starter-data-r2dbc")
    implementation("org.springframework.boot:spring-boot-starter-aop")
    runtimeOnly("io.asyncer:r2dbc-mysql:1.0.4")
    api(libs.springdoc.webflux.ui)
}
