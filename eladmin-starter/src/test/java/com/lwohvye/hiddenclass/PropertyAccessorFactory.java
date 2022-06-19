/*
 *    Copyright (c) 2021-2022.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.hiddenclass;

import org.objectweb.asm.*;

import java.beans.Introspector;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodHandles.Lookup;

import static java.lang.invoke.MethodType.methodType;
import static org.springframework.asm.Opcodes.V17;

/**
 * Very basic property accessor generator; doesn't take care of primitive boxing etc.
 * 这是一个代理工厂类，生成代理类的。
 * <a href="http://openjdk.java.net/jeps/371">Hidden Classes</a>
 * <a href="https://github.com/gunnarmorling/hidden-classes">demo</a>
 * <a href="https://www.lwohvye.com/2021/12/27/asm-%e5%ba%93%e7%9a%84%e4%bb%8b%e7%bb%8d%e5%92%8c%e4%bd%bf%e7%94%a8/">ASM相关</a>
 */
public class PropertyAccessorFactory implements Opcodes {

    // Hidden Classes。为sun.misc.Unsafe::defineAnonymousClass 的部分功能的替代
    @SuppressWarnings("unchecked")
    public static <T> PropertyAccessor<T> getPropertyAccessor(Class<T> clazz) {
        Lookup lookup;
        try {
            lookup = MethodHandles.privateLookupIn(clazz, MethodHandles.lookup())
                    // 需注意虽然这里用了ASM，但隐藏类本身与ASM不是绑定的，这点不要误解了。
                    // 虽未细看，但隐藏类较传统代理类应该是有一定优势的，比如类的生命周期较短、且无法被反射，且用于替代不建议使用的Unsafe类相关。具体可以去理解下面的部分，摘自jeps/371
                    // Goals
                    // - Allow frameworks to define classes as non-discoverable implementation details of the framework,so that they cannot be linked against by other classes nor discovered through reflection.
                    // - Support extending an access control nest with non-discoverable classes.
                    // - Support aggressive unloading of non-discoverable classes, so that frameworks have the flexibility to define as many as they need.
                    // - Deprecate the non-standard API sun.misc.Unsafe::defineAnonymousClass, with the intent to deprecate it for removal in a future release.
                    // - Do not change the Java programming language in any way.
                    //
                    // Non-Goals
                    // - It is not a goal to support all the functionality of sun.misc.Unsafe::defineAnonymousClass, such as constant-pool patching.
                    .defineHiddenClass(PropertyAccessorFactory.generatePropertyAccessor(clazz), false); // 通过defineHiddenClass来获取 Hidden Classes
            var ctor = lookup.findConstructor(lookup.lookupClass(), methodType(void.class));
            return (PropertyAccessor<T>) ctor.invoke();
        } catch (Throwable t) {
            throw new RuntimeException(t);
        }
    }

    private static byte[] generatePropertyAccessor(Class<?> clazz) throws Exception {
        var className = clazz.getCanonicalName().replaceAll("\\.", "/");
        var descriptor = clazz.descriptorString();
        var accessorClassName = clazz.getCanonicalName().replaceAll("\\.", "/") + "PropertyAccessor";
        var accessorDescriptor = "L" + clazz.getCanonicalName().replaceAll("\\.", "/") + "PropertyAccessor" + ";";

        var properties = Introspector.getBeanInfo(clazz).getPropertyDescriptors();

        // 下面这些就是ASM的相关内容了。这部分是通过ASMifier自动生成的，
        var classWriter = new ClassWriter(0);
        MethodVisitor methodVisitor;

        classWriter.visit(V17, ACC_PUBLIC | ACC_SUPER, accessorClassName,
                "Ljava/lang/Object;Lcom/lwohvye/hiddenclass/PropertyAccessor<" + descriptor + ">;",
                "java/lang/Object", new String[]{"com/lwohvye/hiddenclass/PropertyAccessor"});

        {
            // init方法
            methodVisitor = classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
            methodVisitor.visitCode();
            var label0 = new Label();
            methodVisitor.visitLabel(label0);
            methodVisitor.visitLineNumber(3, label0); // 这里通过LineNumber访问。理论上可用于在指定位置添加代码。
            methodVisitor.visitVarInsn(ALOAD, 0);
            methodVisitor.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
            methodVisitor.visitInsn(RETURN);
            var label1 = new Label();
            methodVisitor.visitLabel(label1);
            methodVisitor.visitLocalVariable("this", accessorDescriptor, null, label0, label1, 0);
            methodVisitor.visitMaxs(1, 1);
            methodVisitor.visitEnd();
        }
        {
            // getValue(T instance, String property)方法
            methodVisitor = classWriter.visitMethod(ACC_PUBLIC, "getValue",
                    "(" + descriptor + "Ljava/lang/String;)Ljava/lang/Object;", null, null);
            methodVisitor.visitParameter("instance", 0);
            methodVisitor.visitParameter("property", 0);
            methodVisitor.visitCode();
            var label0 = new Label();
            var label1 = new Label();
            var label2 = new Label();
            methodVisitor.visitTryCatchBlock(label0, label1, label2, "java/lang/Throwable");

            // var field = instance.getClass().getDeclaredField(property);
            methodVisitor.visitLabel(label0);
            methodVisitor.visitLineNumber(35, label0); // 行号这里，只要顺序没问题就行
            methodVisitor.visitVarInsn(ALOAD, 1);
            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "getClass", "()Ljava/lang/Class;", false);
            methodVisitor.visitVarInsn(ALOAD, 2);
            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Class", "getDeclaredField", "(Ljava/lang/String;)Ljava/lang/reflect/Field;", false);
            methodVisitor.visitVarInsn(ASTORE, 3);

            // field.trySetAccessible();
            var label3 = new Label();
            methodVisitor.visitLabel(label3);
            methodVisitor.visitLineNumber(36, label3);
            methodVisitor.visitVarInsn(ALOAD, 3);
            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/reflect/Field", "trySetAccessible", "()Z", false);
            methodVisitor.visitInsn(POP);

            // return field.get(instance);
            var label4 = new Label();
            methodVisitor.visitLabel(label4);
            methodVisitor.visitLineNumber(37, label4);
            methodVisitor.visitVarInsn(ALOAD, 3);
            methodVisitor.visitVarInsn(ALOAD, 1);
            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/reflect/Field", "get", "(Ljava/lang/Object;)Ljava/lang/Object;", false);
            methodVisitor.visitLabel(label1);
            methodVisitor.visitInsn(ARETURN);

            methodVisitor.visitLabel(label2);
            methodVisitor.visitLineNumber(27, label2);
            methodVisitor.visitFrame(Opcodes.F_SAME1, 0, null, 1, new Object[]{"java/lang/Throwable"});
            methodVisitor.visitVarInsn(ASTORE, 3);
            var label5 = new Label();
            methodVisitor.visitLabel(label5);
            methodVisitor.visitVarInsn(ALOAD, 3);
            methodVisitor.visitInsn(ATHROW);

            var label6 = new Label();
            methodVisitor.visitLabel(label6);
            methodVisitor.visitLocalVariable("field", "Ljava/lang/reflect/Field;", null, label3, label2, 3);
            methodVisitor.visitLocalVariable("$ex", "Ljava/lang/Throwable;", null, label5, label6, 3);
            methodVisitor.visitLocalVariable("this", accessorDescriptor, null, label0, label6, 0);
            methodVisitor.visitLocalVariable("instance", "" + descriptor + "", null, label0, label6, 1);
            methodVisitor.visitLocalVariable("property", "Ljava/lang/String;", null, label0, label6, 2);
            methodVisitor.visitMaxs(2, 4);
            methodVisitor.visitEnd();
        }
        {
            // getValue(Object instance, String property)
            methodVisitor = classWriter.visitMethod(ACC_PUBLIC | ACC_BRIDGE | ACC_SYNTHETIC, "getValue",
                    "(Ljava/lang/Object;Ljava/lang/String;)Ljava/lang/Object;", null, null);
            methodVisitor.visitParameter("instance", ACC_SYNTHETIC);
            methodVisitor.visitParameter("property", ACC_SYNTHETIC);
            methodVisitor.visitCode();
            var label0 = new Label();
            methodVisitor.visitLabel(label0);
            methodVisitor.visitLineNumber(1, label0);
            methodVisitor.visitVarInsn(ALOAD, 0);
            methodVisitor.visitVarInsn(ALOAD, 1);
            methodVisitor.visitTypeInsn(CHECKCAST, className);
            methodVisitor.visitVarInsn(ALOAD, 2);
            // 这里是直接调用了上面的getValue。感觉当前逻辑下，是不会走到这个方法的
            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, accessorClassName, "getValue", "(" + descriptor + "Ljava/lang/String;)Ljava/lang/Object;", false);
            methodVisitor.visitInsn(ARETURN);
            var label1 = new Label();
            methodVisitor.visitLabel(label1);
            methodVisitor.visitLocalVariable("this", accessorDescriptor, null, label0, label1, 0);
            methodVisitor.visitMaxs(3, 3);
            methodVisitor.visitEnd();
        }
        classWriter.visitEnd();

        return classWriter.toByteArray();
    }
}
