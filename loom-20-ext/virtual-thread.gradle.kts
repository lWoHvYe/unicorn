import org.springframework.boot.gradle.plugin.SpringBootPlugin

dependencies {
    implementation(platform(SpringBootPlugin.BOM_COORDINATES))
    implementation("org.springframework.boot:spring-boot-starter")
}
