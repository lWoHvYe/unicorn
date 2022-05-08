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

package com.lwohvye;

import com.lwohvye.hiddenclass.Customer;
import com.lwohvye.hiddenclass.PropertyAccessorFactory;
import lombok.SneakyThrows;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import javax.script.ScriptEngineManager;
import java.io.FileReader;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.LongAdder;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class EladminSystemApplicationTests {

    @Test
    public void contextLoads() {
    }

    @SneakyThrows
    public static void main(String[] args) {
        final var friend = new Friend("在一起", 8);
        var person = new Person(1L, "咸鱼", 1, 12, true, 18.0F, friend, new ListNode(10));
        final var name = person.name();
        final var age = person.age();
//        隐式的继承自Record类。有equals hashCode toString方法
        final var equals = person.equals(person);
        final var string = person.toString();

        final var next = person.listNode().next;
        final var friends = Person.friends;

        var m = new HashMap<String, Long>();

        var cm0 = new ConcurrentHashMap<String, Long>();
        cm0.put("H", 0L); // 里面要有H，且与oldVal一致。不然下面的replace永远走不出去
        var cm1 = new ConcurrentHashMap<String, LongAdder>();
        var cm2 = new ConcurrentHashMap<String, Long>();
        var cm3 = new ConcurrentHashMap<String, Long>();
        for (int i = 0; i < 3; i++) {
            m.put("H", m.getOrDefault("H", 0L) + 1); // 获取散列集中H的值，并加1

            Long oldVal;
            long newVal;
            do {
                oldVal = cm0.get("H");
                newVal = oldVal == null ? 1 : oldVal + 1;
            } while (!cm0.replace("H", oldVal, newVal)); // 用replace来循环实现。

            // LongAdder可以支持并发环境
            cm1.putIfAbsent("H", new LongAdder()); // 需要先调用这个
            cm1.get("H").increment(); // 不存在时再设置

            // 下面这俩compute和merge是原子操作
            cm2.compute("H", (k, v) -> Objects.nonNull(v) ? 1 + v : 1); // 将函数应用到key和get(key),将key与结果关联,如果结果为 null,则删除这个键。返回get(key)

            cm3.merge("H", 1L, Long::sum); // 如果key与一个非null值v关联,将函数应用到v和value,将key与结果关联,如果结果为null,则删除这个键。否则，将key与函数的结果关联，返回get(key)
        }


        Set<String> words = ConcurrentHashMap.newKeySet(); // 并发集视图。从cm而来，key为并发集，val为Boolean.TRUE
        Set<String> words2 = new ConcurrentHashMap<String, Long>().keySet(1L); // 另一种并发集视图，key为并发集，val为1L

        List<String> syncArrayList = Collections.synchronizedList(new ArrayList<>()); // 对于经常被修改的数组列表， 同步的 ArrayList 可以胜过 CopyOnWriteArrayList()；
        Map<String, Long> syncHashMap = Collections.synchronizedMap(new HashMap<>()); // 对于映射，ConcurrentHashMap较同步的HashMap要好一些。

        var pMap = Stream.of(person, person, person).collect(Collectors.toMap(Person::name, Function.identity())); // 用identity比 t -> t 看着舒服

        // ----------------------------------------------------------------------------------------------
        // 获取脚本引擎
        var manager = new ScriptEngineManager();
        var nashornEngine = manager.getEngineByName("nashorn"); // js脚本引擎
        var jsScript = ""; // js脚本
        var result0 = nashornEngine.eval(jsScript); // 执行脚本-来自字符串
        var jsFile = new FileReader("fileName");
        var result1 = nashornEngine.eval(jsFile); // 执行脚本-来自文件
        // 还可以设置或获取域内的属性。这里略过
        // 可以指定结果的输出
        // 还可以调用脚本的函数和方法。
        // ----------------------------------------------------------------------------------------------
        // MethodHandle为1.7引入的。Unsafe 是不建议开发者直接使用的，因为 Unsafe 所操作的并不属于Java标准，会容易带来一些安全性的问题。
        // JDK9 之后，官方推荐使用 java.lang.invoke.VarHandle 来替代 Unsafe 大部分功能，对比 Unsafe ，VarHandle 有着相似的功能，但会更加安全，并且，在并发方面也提高了不少性能。
        // https://www.lwohvye.com/2021/12/26/juc%e6%95%b4%e7%90%86%e7%ac%94%e8%ae%b0-varhandle/
        var treeNode = new TreeNode(1);
        var bgVarHandle = MethodHandles.lookup().in(TreeNode.class).findVarHandle(TreeNode.class, "val", int.class);
        System.out.println("get：" + bgVarHandle.get(treeNode));
        bgVarHandle.set(treeNode, 2);

        var nameVarHandle = MethodHandles.privateLookupIn(Person.class, MethodHandles.lookup()).findVarHandle(Person.class, "name", String.class);
        System.out.println("get：" + nameVarHandle.get(person));
        // record中是final属性，所以下面的两个set会报错的
        nameVarHandle.set(person, "社会主义");
        System.out.println(person.name());
        nameVarHandle.setVolatile(person, "核心价值观");
        System.out.println(person.name());
        // ------------------------------------------------------------------------------------------------
    }

    //    Person person;

    @Test
    public void testHiddenClass() throws Throwable {
        // com.lwohvye.hiddenclass.CustomerPropertyAccessor/0x0000000800ca8c00 类名是这样的，最后用 /分隔，后面是Hidden Classes
        // var accessor = PropertyAccessorFactory.getPropertyAccessor(Customer.class);
        //
        // var customer = new Customer("Idol", 18L, LocalDate.of(1995, Month.MAY, 23), "Main Street");
        // assertEquals("Idol", accessor.getValue(customer, "name"));
        // assertEquals(LocalDate.of(1995, Month.MAY, 23), accessor.getValue(customer, "birthday"));
        // assertEquals("Main Street", accessor.getValue(customer, "address"));

        var friend = new Friend("在一起", 8);
        var person = new Person(1L, "咸鱼", 1, 12, true, 18.0F, friend, new ListNode(10));
        System.out.println("person.name() = " + person.name()); // 通过这种方式访问属性
        // MethodHandle和VarHandle
        var nameVarHandle = MethodHandles.privateLookupIn(Person.class, MethodHandles.lookup()).findVarHandle(Person.class, "name", String.class);
        System.out.println("get：" + nameVarHandle.get(person));
        // 对私有方法，用privateLookupIn。下面这个findSpecial还没搞明白
        // System.out.println(MethodHandles.lookup().findSpecial(Person.class, "doJoy", MethodType.methodType(String.class), Person.class).invoke(person));
        System.out.println(MethodHandles.privateLookupIn(Person.class, MethodHandles.lookup()).findVirtual(Person.class, "doJoy", MethodType.methodType(String.class)).invoke(person));
        System.out.println("-----------------");
        // 下面这三行算是一个使用场景，获取类对象的方式有很多，这算是比较复杂的一种情况了
        // 首先要能拿到类对象
        Class<?> aClass = Class.forName("com.lwohvye.Person");
        // 目标方法参数也是已知的。
        var methodType = MethodType.methodType(String.class, new Class[]{Integer.class});
        // 用invoke时可能还有向上转型之类的
        System.out.println(MethodHandles.privateLookupIn(aClass, MethodHandles.lookup()).findVirtual(aClass, "haveJoy", methodType).invoke(person, Person.joy()));

        // 这里不支持向上转型，比如参数是Number的话，用Long去找是找不到的
        // var methodType2 = MethodType.methodType(String.class, new Class[]{Long.class});
        var methodType2 = MethodType.methodType(String.class, new Class[]{Number.class});
        // 用invoke时支持向上转型之类的。参数是Number类型，传其子类进去是可以的。这两点还是比较容易理解的
        System.out.println(MethodHandles.privateLookupIn(aClass, MethodHandles.lookup()).findVirtual(aClass, "haveJoy", methodType2).invoke(person, Long.parseLong(String.valueOf(Person.joy()))));
        System.out.println(MethodHandles.privateLookupIn(aClass, MethodHandles.lookup()).findVirtual(aClass, "haveJoy", methodType2).invoke(person, Person.joy()));

        // 返回值这里是不支持向上/向下转型的
        // var methodType3 = MethodType.methodType(Object.class, new Class[]{Integer.class, String.class});
        // System.out.println(MethodHandles.privateLookupIn(aClass, MethodHandles.lookup()).findVirtual(aClass, "haveJoy", methodType3).invoke(person, Person.joy(), "up/down"));
        // var methodType4 = MethodType.methodType(String.class, new Class[]{Long.class, String.class});
        // System.out.println(MethodHandles.privateLookupIn(aClass, MethodHandles.lookup()).findVirtual(aClass, "haveJoy", methodType4).invoke(person, Long.parseLong(String.valueOf(Person.joy())), "up/down"));

        // 与MethodHandle相比，反射的确更简单。但效率上反射低的有点多了
        var haveJoyMethod = aClass.getDeclaredMethod("haveJoy", Integer.class, String.class);
        haveJoyMethod.trySetAccessible(); // 反射对非public的要加这一步
        System.out.println(haveJoyMethod.invoke(person, Person.joy(), "byReflect"));
        // 反射这块也不支持向下转型
        // var haveJoyMethod2 = aClass.getDeclaredMethod("haveJoy", Number.class, String.class);
        // 向上转型也不支持
        // var haveJoyMethod3 = aClass.getDeclaredMethod("haveJoy", Long.class);

        System.out.println();
        // 访问public方法
        System.out.println(MethodHandles.lookup().findVirtual(Person.class, "name", MethodType.methodType(String.class)).invoke(person));
        // nameVarHandle.set(person, "试一试"); //因为Person是record，使得无法通过这种方式重新设置值，即便用IMPL_LOOKUP也不行，因为返回的VarHandle是ReadOnly的，`this.allowedModes == TRUSTED && !getField.isTrustedFinalField()` 针对于record，第二个为 `!true` ，更换lookup只影响第一个
        // DynamicEnumHelper.unsafe.putObject(person,DynamicEnumHelper.unsafe.objectFieldOffset(Person.class.getDeclaredField("name")),"试一试"); // Unsafe: can't get field offset on a record class
        if (Person.class.isRecord()) {
            for (var recordComponent : Person.class.getRecordComponents()) {
                System.out.println(recordComponent);
            }
        }

        var accessor = PropertyAccessorFactory.getPropertyAccessor(Person.class);
        assertEquals(person.name(), accessor.getValue(person, "name"));
        assertEquals(person.age(), accessor.getValue(person, "age"));

        assertTrue(accessor.getClass().isHidden());
        assertNull(accessor.getClass().getCanonicalName());
    }

    @Test
    public void testCannotLoadHiddenClass() throws Throwable {
        var accessor = PropertyAccessorFactory.getPropertyAccessor(Customer.class);
        // 这里会报类找不到，因为隐藏类是不能被ClassLoader load的
        accessor.getClass().getClassLoader().loadClass(accessor.getClass().getName());
    }
}

