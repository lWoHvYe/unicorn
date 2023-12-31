/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.core.utils;

import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockMultipartFile;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;

import static com.lwohvye.core.utils.FileUtils.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class FileUtilsTest {

    @Test
    public void testToFile() {
        long retval = toFile(new MockMultipartFile("foo", (byte[]) null)).getTotalSpace();
        assertEquals(500695072768L, retval);
    }

    @Test
    public void testGetExtensionName() {
        assertEquals("foo", getExtensionName("foo"));
        assertEquals("exe", getExtensionName("bar.exe"));
    }

    @Test
    public void testGetFileNameNoEx() {
        assertEquals("foo", getFileNameNoEx("foo"));
        assertEquals("bar", getFileNameNoEx("bar.txt"));
    }

    @Test
    public void testGetSize() {
        assertEquals("1000B   ", getSize(1000));
        assertEquals("1.00KB   ", getSize(1024));
        assertEquals("1.00MB   ", getSize(1048576));
        assertEquals("1.00GB   ", getSize(1073741824));
    }

    @Test
    public void testLoadClass() throws Exception {
        String jarAddress = "/Users/lWoHvYe/IdeaProjects/unicorn/unicorn-core/build/libs/unicorn-core-4.2.0-pi-RC2.jar";
        // jar的Url路径为jarPath，jarPath = "file:" + jarAddress。也可以这样：
        var file = new File(jarAddress);
        var jarPath = file.toURI().toURL();
        try (var urlClassLoader = new URLClassLoader(new URL[]{jarPath}, Thread.currentThread().getContextClassLoader())) {
            var classNameSet = SpringContextHolder.readJarFile(jarAddress);
            for (var className : classNameSet) {
                var clazz = urlClassLoader.loadClass(className);
                System.out.println(clazz.getName());
            }
        }

        // 因为测试类在common中，这里如果读远程的common的化，可能要结果不那么突出，因为本地的也可以被加载，所以改读api了
        jarPath = new URL("https://repo1.maven.org/maven2/com/lwohvye/unicorn-sys-api/4.2.0-pi-RC1/unicorn-core-4.2.0-pi-RC2.jar");
        try (var urlClassLoader = new URLClassLoader(new URL[]{jarPath}, Thread.currentThread().getContextClassLoader())) {
            Class<?> aClass = urlClassLoader.loadClass("com.lwohvye.api.modules.system.api.SysUserAPI"); // 3.0.3开始的结构，读3.0.2的jar会报类找不到，这是正确的
//            Class<?> aClass = urlClassLoader.loadClass("com.lwohvye.modules.system.api.SysUserAPI"); // 3.0.2时的结构
            System.out.println(aClass.descriptorString());
        }
    }
}
