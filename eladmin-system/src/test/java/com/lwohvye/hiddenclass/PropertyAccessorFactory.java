/*
 *    Copyright (c) 2021.  lWoHvYe(Hongyan Wang)
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

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.beans.Introspector;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodHandles.Lookup;

import static java.lang.invoke.MethodType.methodType;

/**
 * Very basic property accessor generator; doesn't take care of primitive boxing etc.
 * <a href="http://openjdk.java.net/jeps/371" />
 * <a href="https://github.com/gunnarmorling/hidden-classes" />
 */
public class PropertyAccessorFactory implements Opcodes {

    // Hidden Classes。为sun.misc.Unsafe::defineAnonymousClass 的部分功能的替代
    @SuppressWarnings("unchecked")
    public static <T> PropertyAccessor<T> getPropertyAccessor(Class<T> clazz) {
        Lookup lookup;
        try {
            lookup = MethodHandles.privateLookupIn(clazz, MethodHandles.lookup())
                    .defineHiddenClass(PropertyAccessorFactory.generatePropertyAccessor(clazz), false); // 通过defineHiddenClass来获取

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

        // 下面这些就是ASM的相关内容了
        var classWriter = new ClassWriter(0);
        MethodVisitor methodVisitor;

        classWriter.visit(V13, ACC_PUBLIC | ACC_SUPER, accessorClassName,
                "Ljava/lang/Object;Lcom/lwohvye/hiddenclass/PropertyAccessor<" + descriptor + ">;",
                "java/lang/Object", new String[]{"com/lwohvye/hiddenclass/PropertyAccessor"});

        {
            // init方法
            methodVisitor = classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
            methodVisitor.visitCode();
            var label0 = new Label();
            methodVisitor.visitLabel(label0);
            methodVisitor.visitLineNumber(3, label0);
            methodVisitor.visitVarInsn(ALOAD, 0);
            methodVisitor.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
            methodVisitor.visitInsn(RETURN);
            var label1 = new Label();
            methodVisitor.visitLabel(label1);
            methodVisitor.visitLocalVariable("this", accessorDescriptor, null, label0, label1, 0);
            methodVisitor.visitMaxs(1, 1);
            methodVisitor.visitEnd();
        }
        // 下面这部分是如何获取属性的，也要再看看
        {
            // getValue方法
            methodVisitor = classWriter.visitMethod(ACC_PUBLIC, "getValue",
                    "(" + descriptor + "Ljava/lang/String;)Ljava/lang/Object;", null, null);
            methodVisitor.visitCode();
            var label0 = new Label();
            methodVisitor.visitLabel(label0);

            for (var propertyDescriptor : properties) {
                if (propertyDescriptor.getName().equals("class"))
                    continue;

                methodVisitor.visitFrame(Opcodes.F_SAME, 0, null, 0, null);
                methodVisitor.visitVarInsn(ALOAD, 2);
                methodVisitor.visitLdcInsn(propertyDescriptor.getName());
                methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "equals", "(Ljava/lang/Object;)Z", false);
                var label1 = new Label();
                methodVisitor.visitJumpInsn(IFEQ, label1);
                var label2 = new Label();
                methodVisitor.visitLabel(label2);
                methodVisitor.visitVarInsn(ALOAD, 1);
                methodVisitor.visitMethodInsn(INVOKEVIRTUAL, className, propertyDescriptor.getReadMethod().getName(),
                        "()" + propertyDescriptor.getReadMethod().getReturnType().descriptorString(), false);
                methodVisitor.visitInsn(ARETURN);
                methodVisitor.visitLabel(label1);
            }
//            被注释这块是原始的方式，后面采用了新的方式，就是下面的
//            // name
//            methodVisitor.visitLineNumber(7, label0);
//            methodVisitor.visitFrame(Opcodes.F_SAME, 0, null, 0, null);
//            methodVisitor.visitVarInsn(ALOAD, 2);
//            methodVisitor.visitLdcInsn("name");
//            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "equals", "(Ljava/lang/Object;)Z", false);
//            Label label1 = new Label();
//            methodVisitor.visitJumpInsn(IFEQ, label1);
//            Label label2 = new Label();
//            methodVisitor.visitLabel(label2);
//            methodVisitor.visitLineNumber(8, label2);
//            methodVisitor.visitVarInsn(ALOAD, 1);
//            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, className, "getName",
//                    "()Ljava/lang/String;", false);
//            methodVisitor.visitInsn(ARETURN);
//            methodVisitor.visitLabel(label1);
//
//            // age
//            methodVisitor.visitLineNumber(11, label1);
//            methodVisitor.visitFrame(Opcodes.F_SAME, 0, null, 0, null);
//            methodVisitor.visitVarInsn(ALOAD, 2);
//            methodVisitor.visitLdcInsn("age");
//            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "equals", "(Ljava/lang/Object;)Z", false);
//            Label label3 = new Label();
//            methodVisitor.visitJumpInsn(IFEQ, label3);
//            Label label4 = new Label();
//            methodVisitor.visitLabel(label4);
//            methodVisitor.visitLineNumber(12, label4);
//            methodVisitor.visitVarInsn(ALOAD, 1);
//            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, className, "getAge", "()I",
//                    false);
//            methodVisitor.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;",
//                    false);
//            methodVisitor.visitInsn(ARETURN);
//            methodVisitor.visitLabel(label3);
//
//            // address
//            methodVisitor.visitLineNumber(15, label3);
//            methodVisitor.visitFrame(Opcodes.F_SAME, 0, null, 0, null);
//            methodVisitor.visitVarInsn(ALOAD, 2);
//            methodVisitor.visitLdcInsn("address");
//            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "equals", "(Ljava/lang/Object;)Z", false);
//            Label label5 = new Label();
//            methodVisitor.visitJumpInsn(IFEQ, label5);
//            Label label6 = new Label();
//            methodVisitor.visitLabel(label6);
//            methodVisitor.visitLineNumber(16, label6);
//            methodVisitor.visitVarInsn(ALOAD, 1);
//            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, className, "getAddress",
//                    "()Ljava/lang/String;", false);
//            methodVisitor.visitInsn(ARETURN);
//            methodVisitor.visitLabel(label5);

//            methodVisitor.visitLineNumber(19, label5);
            methodVisitor.visitFrame(Opcodes.F_SAME, 0, null, 0, null);
            methodVisitor.visitTypeInsn(NEW, "java/lang/IllegalArgumentException");
            methodVisitor.visitInsn(DUP);
            methodVisitor.visitTypeInsn(NEW, "java/lang/StringBuilder");
            methodVisitor.visitInsn(DUP);
            methodVisitor.visitLdcInsn("Unknown property: ");
            methodVisitor.visitMethodInsn(INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "(Ljava/lang/String;)V", false);
            methodVisitor.visitVarInsn(ALOAD, 2);
            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append",
                    "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false);
            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;", false);
            methodVisitor.visitMethodInsn(INVOKESPECIAL, "java/lang/IllegalArgumentException", "<init>",
                    "(Ljava/lang/String;)V", false);
            methodVisitor.visitInsn(ATHROW);
            var label7 = new Label();
            methodVisitor.visitLabel(label7);
            methodVisitor.visitLocalVariable("this", accessorDescriptor, null, label0, label7, 0);
            methodVisitor.visitLocalVariable("instance", descriptor, null, label0, label7, 1);
            methodVisitor.visitLocalVariable("property", "Ljava/lang/String;", null, label0, label7, 2);
            methodVisitor.visitMaxs(5, 3);
            methodVisitor.visitEnd();
        }
        {
            methodVisitor = classWriter.visitMethod(ACC_PUBLIC | ACC_BRIDGE | ACC_SYNTHETIC, "getValue",
                    "(Ljava/lang/Object;Ljava/lang/String;)Ljava/lang/Object;", null, null);
            methodVisitor.visitCode();
            var label0 = new Label();
            methodVisitor.visitLabel(label0);
            methodVisitor.visitLineNumber(1, label0);
            methodVisitor.visitVarInsn(ALOAD, 0);
            methodVisitor.visitVarInsn(ALOAD, 1);
            methodVisitor.visitTypeInsn(CHECKCAST, className);
            methodVisitor.visitVarInsn(ALOAD, 2);
            methodVisitor.visitMethodInsn(INVOKEVIRTUAL, accessorClassName, "getValue", "(" + descriptor + "Ljava/lang/String;)Ljava/lang/Object;", false);
            methodVisitor.visitInsn(ARETURN);
            methodVisitor.visitMaxs(3, 3);
            methodVisitor.visitEnd();
        }
        classWriter.visitEnd();

        return classWriter.toByteArray();
    }
}
