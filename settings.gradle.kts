/*
 *    Copyright (c) 2024.  lWoHvYe(Hongyan Wang)
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

/*
 * This file was generated by the Gradle 'init' task.
 */
pluginManagement {
    // Include 'plugins build' to define convention plugins.
    includeBuild("build-logic")
    repositories {
        gradlePluginPortal()
    }
}

plugins {
    id("com.gradle.develocity") version "3.17.4"
    id("org.gradle.toolchains.foojay-resolver-convention") version ("0.8.0")
}

dependencyResolutionManagement {
    repositoriesMode.set(RepositoriesMode.FAIL_ON_PROJECT_REPOS)
    repositories {
        gradlePluginPortal()
        flatDir {
            dirs(rootProject.projectDir.resolve("ex-lib"))
        }
    }

    versionCatalogs {
        create("libs") {
        }
    }
}

rootProject.name = "valentine-p2p"

val buildFiles = fileTree(rootDir) {
    val excludes = gradle.startParameter.projectProperties["excludeProjects"]?.split(",")
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
    if (excludes != null) {
        exclude(*excludes.toTypedArray())
    }
}

buildFiles.forEach { buildFile ->
    val isDefaultName = buildFile.name == "build.gradle" || buildFile.name == "build.gradle.kts"
    val isKotlin = buildFile.name.endsWith(".kts")

    if (isDefaultName) {
        val buildFilePath = buildFile.parentFile.absolutePath
        val projectPath = buildFilePath.replace(rootDir.absolutePath, "").replace(File.separator, ":")
        include(projectPath)
    } else {
        val projectName =
            if (isKotlin) {
                buildFile.name.replace(".gradle.kts", "")
            } else {
                buildFile.name.replace(".gradle", "")
            }

        val projectPath = ":$projectName"
        include(projectPath)

        val project = findProject(projectPath)
        project?.name = projectName
        project?.projectDir = buildFile.parentFile
        project?.buildFileName = buildFile.name
    }
}

develocity {
    buildScan {
        publishing.onlyIf { providers.environmentVariable("CI").getOrElse("false").toBoolean() }
        termsOfUseUrl.set("https://gradle.com/terms-of-service")
        termsOfUseAgree.set("yes")
    }
}
