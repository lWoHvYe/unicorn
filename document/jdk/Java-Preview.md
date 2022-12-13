#### Project Loom（Preview in Java 19）

- [JEP 425: Virtual Threads](https://openjdk.java.net/jeps/425)
- [JEP 436: Virtual Threads (Second Preview)](https://openjdk.org/jeps/436)
- [Project Loom](https://wiki.openjdk.java.net/display/loom)
- [Project Loom: Fibers and Continuations for the Java Virtual Machine](http://cr.openjdk.java.net/~rpressler/loom/Loom-Proposal.html)
- 2020-04-28更新，`JEP 425:Virtual Threads (Preview)` 在Java 19 Preview， review ends: 2022/05/05

- 几个值得单独拉出来的改动(在JEP 425 - Dependencies)：
    - [JEP 416 (Reimplement Core Reflection with Method Handles)](https://openjdk.java.net/jeps/416) in JDK 18 removed
      the VM-native reflection implementation.
      This allows virtual threads to park gracefully when methods are invoked reflectively.
    - [JEP 353 (Reimplement the Legacy Socket API)](https://openjdk.java.net/jeps/353) in JDK 13,
      and [JEP 373 (Reimplement the Legacy DatagramSocket API)](https://openjdk.java.net/jeps/373) in JDK 15, replaced
      the implementations of `java.net.Socket`
      , `ServerSocket`, and `DatagramSocket` with new implementations designed for use with virtual threads.
    - [JEP 418 (Internet-Address Resolution SPI)](https://openjdk.java.net/jeps/418) in JDK 18 defined a
      service-provider interface for host name and address
      lookup. This will allow third-party libraries to implement alternative `java.net.InetAddress` resolvers that do
      not pin threads during host lookup.

- In general, the fiber API will be nearly identical to that of Thread as the abstraction is the same, and we'd also
  like to run code that so far has run in
  kernel threads to run in fibers with little or no modification. This immediately suggests two design options:
    - Represent fibers as a Fiber class, and factor out the common API for Fiber and Thread into a common super-type,
      provisionally called Strand.
      Thread-implementation-agnostic code would be programmed against Strand, so that Strand.currentStrand would return
      a fiber if the code is running in a
      fiber, and Strand.sleep would suspend the fiber if the code is running in a fiber.
    - Use the same Thread class for both kinds of threads — user-mode and kernel-mode — and choose an implementation as
      a dynamic property set in a constructor
      or a setter called prior to invoking start.
    - A separate Fiber class might allow us more flexibility to deviate from Thread, but would also present some
      challenges. Because a user-mode scheduler does
      not have direct access to CPU cores, assigning a fiber to a core is done by running it in some worker kernel
      thread, and so every fiber has an underlying
      kernel thread, at least while it is scheduled to a CPU core, although the identity of underlying kernel thread is
      not fixed, and may change if the
      scheduler decides to schedule the same fiber to a different worker kernel thread. If the scheduler is written in
      Java — as we want — every fiber even has
      an underlying Thread instance. If fibers are represented by the Fiber class, the underlying Thread instance would
      be accessible to code running in a
      fiber (e.g. with Thread.currentThread or Thread.sleep), which seems inadvisable.
    - If fibers are represented by the same Thread class, a fiber's underlying kernel thread would be inaccessible to
      user code, which seems reasonable but has
      a number of implications. For one, it would require more work in the JVM, which makes heavy use of the Thread
      class, and would need to be aware of a
      possible fiber implementation. **For another, it would limit our design flexibility**. It also creates some
      circularity when writing schedulers, that need
      to
      implement threads (fibers) by assigning them to threads (kernel threads). This means that we would need to expose
      the fiber's (represented by Thread)
      continuation for use by the scheduler.
    - Because fibers are scheduled by Java schedulers, they need not be GC roots, as at any given time a fiber is either
      runnable, in which case a reference to
      it is held by its scheduler, or blocked, in which case a reference to it is held by the object on which it is
      blocked (e.g. a lock or an IO queue), so
      that it can be unblocked. （Fiber相关的，只有在结束运行后，被GC掉）
    - Loom有两种实现方式，一种是抽取公共父类，与Thread平级。另一种是作为Thread的一部分。每一种都有优缺点。既然最终的实现叫Virtual
      Thread，且将其相关加入到Thread的API中，显然用的第二种方式(In the current prototype, virtual threads
      are implemented by the java.lang.Thread API.)
    - The current prototype implements the mount/dismount operations by copying stack frames from the continuation
      stack – stored on the Java heap as two Java
      arrays, an Object array for the references on the stack and a primitive array for primitive values and metadata.
      Copying a frame from the thread stack (
      which we also call the vertical stack, or the v-stack) to the continuation stack (also, the horizontal stack, or
      the h-stack) is called freezing it, while
      copying a frame from the h-stack to the v-stack is called thawing. The prototype also optionally thaws just a
      small portion of the h-stack when mounting
      using an approach called lazy copy; see the JVMLS 2018 talk as well as the section on performance for more
      detail.（针对线程的Stack，采用的方式是yield时，copy
      stack,
      stored on the Java heap）
- [Virtual Threads in Spring 6.x](https://spring.io/blog/2022/10/11/embracing-virtual-threads)
- [Blog-Understanding Java's Project Loom](https://www.marcobehler.com/guides/java-project-loom?mkt_tok=NDI2LVFWRC0xMTQAAAGIcjkwHcDNBFot5rdRdBEUuF6VoChWteoULzKapDGmwmAvhMcx0grhQ0louho-dN1ckoHsIo1dWoRkkUbuaEtY9jNg8gRmb1XxVmmNrLmADNkSKVgN)
- [IntelliJ IDEA Conf 2022 | Project Loom: Revolution in Concurrency or Obscure Implementation Detail?](https://www.youtube.com/watch?v=0DUlUzqr09I)

通过JProfiler可以查看Thread的情况，这一点不得不说收费的就是不一样。VisualVM和jconsole都看不到类型信息。
可以看的基本的main Thread，及19后听到的CarrierThreads和VirtualThreads

![](../images/Threads%20wtb%20.png)

#### Foreign Function & Memory API

- Introduce an API by which Java programs can interoperate with code and data outside of the Java runtime.
- [JEP 424: Foreign Function & Memory API](https://openjdk.java.net/jeps/424)
- 主体是针对 JNI 及 Unsafe (accessing foreign memory) 这两块，提供了新的实现，跟Virtual Threads相关应该也有些关系

#### Structured Concurrency (Incubator)

- Simplify multithreaded programming by introducing a library for structured concurrency. Structured concurrency treats
  multiple tasks running in different
  threads as a single unit of work, thereby streamlining error handling and cancellation, improving reliability, and
  enhancing observability.
- [JEP 428: Structured Concurrency (Incubator)](https://openjdk.java.net/jeps/428)
- [JEP 437: Structured Concurrency (Second Incubator)](https://openjdk.org/jeps/437)
- 多线程相关的，诸如 ExecutorService and Future 这些，实际上跟Virtual Threads也有些相关。主体感觉在Virtual
  Threads提出后，又出现了不少相关的新东西
