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

description = "权限基础模块"

val sharedManifest = rootProject.extra["sharedManifest"] as? Manifest

tasks.jar {
    manifest {
        from(sharedManifest)
        attributes(
            "Implementation-Title" to project.name,
            "Implementation-Version" to project.version,
            "Automatic-Module-Name" to "lwohvye.${project.name.replace("-", ".")}"
        )
    }
    into("META-INF/maven/${project.group}/${project.name}") {
        from({ tasks.named("generatePomFileForMavenJavaSysApiPublication") })
        rename(".*", "pom.xml")
    }
}

publishing {
    publications {
        create<MavenPublication>("mavenJavaSysApi") {
            from(components["java"])
            pom {
                name.set("Unicorn Sys Domain")
                description.set("Domain data for Security module")
            }
        }
    }
}

dependencies {
    api(project(":unicorn-core"))
}
