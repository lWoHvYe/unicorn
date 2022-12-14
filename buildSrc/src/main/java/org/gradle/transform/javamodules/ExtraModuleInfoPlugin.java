/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
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

package org.gradle.transform.javamodules;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.attributes.Attribute;
import org.gradle.api.plugins.JavaPlugin;

/**
 * Entry point of our plugin that should be applied in the root project.
 */
public class ExtraModuleInfoPlugin implements Plugin<Project> {

    @Override
    public void apply(Project project) {
        // register the plugin extension as 'extraJavaModuleInfo {}' configuration block
        var extension = project.getObjects().newInstance(ExtraModuleInfoPluginExtension.class);
        project.getExtensions().add(ExtraModuleInfoPluginExtension.class, "extraJavaModuleInfo", extension);

        // setup the transform for all projects in the build
        project.getPlugins().withType(JavaPlugin.class).configureEach(javaPlugin -> configureTransform(project, extension));
    }

    private void configureTransform(Project project, ExtraModuleInfoPluginExtension extension) {
        var artifactType = Attribute.of("artifactType", String.class);
        var javaModule = Attribute.of("javaModule", Boolean.class);

        // 下面几种是输出log的方式
//        var logger = project.getLogger();
//        logger.warn(" currentProject is {} ", project.getName());
//        System.out.println("normal msg");
//        System.err.println("error msg");

        // compile and runtime classpath express that they only accept modules by requesting the javaModule=true attribute
        project.getConfigurations().matching(this::isResolvingJavaPluginConfiguration).all(
                c -> c.getAttributes().attribute(javaModule, true));

        // all Jars have a javaModule=false attribute by default; the transform also recognizes modules and returns them without modification
        project.getDependencies().getArtifactTypes().getByName("jar").getAttributes().attribute(javaModule, false);

        // register the transform for Jars and "javaModule=false -> javaModule=true"; the plugin extension object fills the input parameter
        project.getDependencies().registerTransform(ExtraModuleInfoTransform.class, t -> {
            t.parameters(parameter -> {
                parameter.setModuleInfo(extension.getModuleInfo());
                parameter.setAutomaticModules(extension.getAutomaticModules());
                parameter.setOverrideModuleInfos(extension.getOverrideModuleInfos());
            });
            t.getFrom().attribute(artifactType, "jar").attribute(javaModule, false);
            t.getTo().attribute(artifactType, "jar").attribute(javaModule, true);
        });
    }

    private boolean isResolvingJavaPluginConfiguration(Configuration configuration) {
        if (!configuration.isCanBeResolved()) {
            return false;
        }
        return configuration.getName().endsWith(JavaPlugin.COMPILE_CLASSPATH_CONFIGURATION_NAME.substring(1))
                || configuration.getName().endsWith(JavaPlugin.RUNTIME_CLASSPATH_CONFIGURATION_NAME.substring(1))
                || configuration.getName().endsWith(JavaPlugin.ANNOTATION_PROCESSOR_CONFIGURATION_NAME.substring(1));
    }
}
