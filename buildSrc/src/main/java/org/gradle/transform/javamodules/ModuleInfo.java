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

import java.io.Serializable;
import java.util.*;

/**
 * Data class to hold the information that should be added as module-info.class to an existing Jar file.
 * <p>
 * 这里的 exports, opens 都是支持 to 的，只是当下没这个需求
 */
public class ModuleInfo implements Serializable {
    private String moduleName;
    private String moduleVersion;
    private List<Tuple2<String, String[]>> exports = new ArrayList<>();
    private List<String> requires = new ArrayList<>();
    private List<String> requiresTransitive = new ArrayList<>();

    private List<Tuple2<String, String[]>> opens = new ArrayList<>();
    private List<String> uses = new ArrayList<>();

    private List<Tuple2<String, String[]>> provides = new ArrayList<>();

    ModuleInfo(String moduleName, String moduleVersion) {
        this.moduleName = moduleName;
        this.moduleVersion = moduleVersion;
    }

    public void exports(String exports, String... to) {
        this.exports.add(Tuple2.of(exports, to));
    }

    public void requires(String requires) {
        this.requires.add(requires);
    }

    public void requiresTransitive(String requiresTransitive) {
        this.requiresTransitive.add(requiresTransitive);
    }

    public void opens(String opens, String... to) {
        this.opens.add(Tuple2.of(opens, to));
    }

    public void uses(String uses) {
        this.uses.add(uses);
    }

    public void provides(String provides, String... with) {
        this.provides.add(Tuple2.of(provides, with));
    }

    public String getModuleName() {
        return moduleName;
    }

    protected String getModuleVersion() {
        return moduleVersion;
    }

    protected List<Tuple2<String, String[]>> getExports() {
        return exports;
    }

    protected List<String> getRequires() {
        return requires;
    }

    protected List<String> getRequiresTransitive() {
        return requiresTransitive;
    }

    protected List<Tuple2<String, String[]>> getOpens() {
        return opens;
    }

    protected List<String> getUses() {
        return uses;
    }

    protected List<Tuple2<String, String[]>> getProvides() {
        return provides;
    }

    @Override
    public String toString() {
        return "ModuleInfo{" +
                "moduleName='" + moduleName + '\'' +
                ", moduleVersion='" + moduleVersion + '\'' +
                ", exports=" + exports +
                ", requires=" + requires +
                ", requiresTransitive=" + requiresTransitive +
                ", opens=" + opens +
                ", uses=" + uses +
                ", provides=" + provides +
                '}';
    }
}
