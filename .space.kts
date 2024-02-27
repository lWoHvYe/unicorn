/**
 * JetBrains Space Automation
 * This Kotlin-script file lets you automate build activities
 * For more info, see https://www.jetbrains.com/help/space/automation.html
 */

job("Assemble & BuildImage") {

    container(displayName = "Run gradle build", image = "eclipse-temurin:21-jdk") {

        kotlinScript { api ->
            // here can be your complex logic
            api.gradlew("assemble")
            api.gradlew("bootBuildImage")
        }
    }
}


