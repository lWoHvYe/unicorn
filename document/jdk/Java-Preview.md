#### Project Loom（Preview in Java 19）

- [JEP 425: Virtual Threads](https://openjdk.java.net/jeps/425)
- [Project Loom](https://wiki.openjdk.java.net/display/loom)
- [Project Loom: Fibers and Continuations for the Java Virtual Machine](http://cr.openjdk.java.net/~rpressler/loom/Loom-Proposal.html)
- 2020-04-28更新，`JEP 425:Virtual Threads (Preview)` 在Java 19 Preview， review ends: 2022/05/05

- 几个值得单独拉出来的改动(在JEP 425 - Dependencies)：
    - [JEP 416 (Reimplement Core Reflection with Method Handles)](https://openjdk.java.net/jeps/416) in JDK 18 removed the VM-native reflection implementation.
      This allows virtual threads to park gracefully when methods are invoked reflectively.
    - [JEP 353 (Reimplement the Legacy Socket API)](https://openjdk.java.net/jeps/353) in JDK 13,
      and [JEP 373 (Reimplement the Legacy DatagramSocket API)](https://openjdk.java.net/jeps/373) in JDK 15, replaced the implementations of `java.net.Socket`
      , `ServerSocket`, and `DatagramSocket` with new implementations designed for use with virtual threads.
    - [JEP 418 (Internet-Address Resolution SPI)](https://openjdk.java.net/jeps/418) in JDK 18 defined a service-provider interface for host name and address
      lookup. This will allow third-party libraries to implement alternative `java.net.InetAddress` resolvers that do not pin threads during host lookup.
