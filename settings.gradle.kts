/*
 *    Copyright (c) 2024-2026.  lWoHvYe(Hongyan Wang)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

pluginManagement {
    includeBuild("build-logic")
    repositories {
        gradlePluginPortal()
    }
}

plugins {
    id("com.gradle.develocity") version "4.5.0"
    id("org.gradle.toolchains.foojay-resolver-convention") version "1.0.0"
}

dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
    repositories {
        mavenCentral()
        flatDir {
            dirs(rootProject.projectDir.resolve("ex-lib"))
        }
    }
}

rootProject.name = "valentine-p2p"

val excludedProjects = providers.gradleProperty("excludeProjects")
    .map { it.split(",") }
    .map { names -> names.filter(String::isNotBlank).toSet() }
    .getOrElse(emptySet())

val buildFiles = fileTree(rootDir) {
    include("**/*.gradle", "**/*.gradle.kts")
    exclude(
        "build",
        "**/gradle",
        "settings.gradle",
        "settings.gradle.kts",
        "buildSrc",
        "build-logic",
        "/build.gradle",
        "/build.gradle.kts",
        ".*",
        "out"
    )

    // Kotlin 2.x did not support Java 26 yet when this workaround was introduced.
    // Keep this disabled for now; re-enable it only if a supported JDK requires
    // excluding the Kotlin modules again.
    // if (!javaVersion.isCompatibleWith(JavaVersion.VERSION_26)) {
    //     exclude("**/*-kotlin.gradle.kts")
    // }

    excludedProjects.forEach(::exclude)
}

buildFiles
    .sortedBy { it.absolutePath }
    .forEach { buildFile ->
        val isDefaultName = buildFile.name == "build.gradle" || buildFile.name == "build.gradle.kts"
        val projectName = buildFile.name
            .removeSuffix(if (buildFile.name.endsWith(".gradle.kts")) ".gradle.kts" else ".gradle")

        if (isDefaultName) {
            val relativePath = buildFile.parentFile.relativeTo(rootDir).path
            val projectPath = ":${relativePath.replace(File.separator, ":")}"
            include(projectPath)
        } else {
            val projectPath = ":$projectName"
            include(projectPath)
            project(projectPath).apply {
                name = projectName
                projectDir = buildFile.parentFile
                buildFileName = buildFile.name
            }
        }
    }

develocity {
    buildScan {
        publishing.onlyIf { providers.environmentVariable("CI").getOrElse("false").toBoolean() }
        termsOfUseUrl.set("https://gradle.com/terms-of-service")
        termsOfUseAgree.set("yes")
    }
}