- [Java Generics](http://www.angelikalanger.com/GenericsFAQ/JavaGenericsFAQ.html)
- [JEP 238: Multi-Release JAR Files](https://openjdk.org/jeps/238)
  其中一个问题是，这个是需要build成jar包才生效的，测试较原来会麻烦一些；
  另一个比较重要的地方是加在这类Jar中Bean上的相关注解会失效（@Component系列，@Configuration系列），从而Bean不会初始化从而报
  bean not found exception
