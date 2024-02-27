/**
 * JetBrains Space Automation
 * This Kotlin-script file lets you automate build activities
 * For more info, see https://www.jetbrains.com/help/space/automation.html
 */

job("Assemble & BuildImage") {
    container(displayName = "Run gradle build", image = "ibm-semeru-runtimes:open-21-jdk") {

        kotlinScript { api ->
            // here can be your complex logic
            api.gradlew("assemble")
            api.gradlew("bootBuildImage")
        }
    }

    container(displayName = "Run gradle build", image = "amazoncorretto:21-alpine") {

        kotlinScript { api ->
            // here can be your complex logic
            api.gradlew("classes")
        }
    }
}


