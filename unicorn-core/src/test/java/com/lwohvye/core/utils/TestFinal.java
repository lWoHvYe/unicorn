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

import static com.lwohvye.core.utils.DynamicEnumHelper.modifyFinalField;

public class TestFinal {

    public static void main(String[] args) throws Exception {

        Person jordan = new Person("Chicago");

        modifyFinalField(jordan, "firstName", "Michael");

        System.out.println(jordan.firstName); //Mike，Debug看到的已经修改成功了，甚至通过反射拿到的也是新值,但实际输出的还是原值

        modifyFinalField(jordan, "lastName", "Michael");

        System.out.println(jordan.lastName); //Michael

        modifyFinalField(jordan, "age", 51f);

        System.out.println(jordan.age); //50.5

        modifyFinalField(jordan, "height", 1.98f);

        System.out.println(jordan.height); //1.98

        modifyFinalField(jordan, "address", new Address("ccc", "ddd"));

        System.out.println(jordan.address.line1); //ccc

        modifyFinalField(jordan, "city", "Miami");

        System.out.println(jordan.city); //Miami
        // 下面两个初步证明static的逻辑与非static的一致
        modifyFinalField(jordan, "sfName", "Michael");
        System.out.println(Person.sfName);//sfName，Debug看到的已经修改成功了，甚至通过反射拿到的也是新值,但实际输出的还是原值

        modifyFinalField(jordan, "sfnName", "Michael");
        System.out.println(Person.sfnName);//Michael

    }
}

class Person {

    public final String firstName = "Mike"; //Literal String不行

    public final String lastName = new String("Jordan"); //非Literal String，可被有效修改

    public final float age = 50.5f; // 普通类型也不行

    public final Float height = 1.99f; //包装类型，可被有效修改

    public final Address address = new Address("aaa", "bbb"); //自定义类型，可被有效修改

    public final String city; //通过构造传入的，也可被有效修改

    public static final String sfName = "sfName";

    public static final String sfnName = new String("sfnName");

    public Person(String city) {

        this.city = city;

    }

}

final class Address {

    public final String line1;

    public final String line2;

    public Address(String line1, String line2) {

        this.line1 = line1;

        this.line2 = line2;

    }

}
