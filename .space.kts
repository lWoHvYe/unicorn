/**
 * JetBrains Space Automation
 * This Kotlin-script file lets you automate build activities
 * For more info, see https://www.jetbrains.com/help/space/automation.html
 */

job("BuildImg") {
    container(displayName = "Run gradle build", image = "amazoncorretto:20-alpine") {

        // env vars for build.gradle
        env["OSSRH_USERNAME"] = "{{ project:ossrh_username }}"
        env["OSSRH_PASSWORD"] = "{{ project:ossrh_password }}"

        kotlinScript { api ->
            // here can be your complex logic
            api.gradlew("build")
        }
    }
}
