/**
 * JetBrains Space Automation
 * This Kotlin-script file lets you automate build activities
 * For more info, see https://www.jetbrains.com/help/space/automation.html
 */

job("BuildImg") {
    container(displayName = "Run gradle build", image = "eclipse-temurin:20-alpine") {

        kotlinScript { api ->
            // here can be your complex logic
            api.gradlew("build --exclude-task test")
        }
    }

    container(displayName = "Run gradle build", image = "amazoncorretto:20-alpine") {

        kotlinScript { api ->
            // here can be your complex logic
            api.gradlew("build --exclude-task test")
        }
    }
}


