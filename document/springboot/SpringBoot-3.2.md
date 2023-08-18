#### Support for Virtual Threads

- Spring Boot 3.2 ships support for virtual threads. To use virtual threads, you need to run on Java 21 and set the
  property spring.threads.virtual.enabled to true.

- Some synchronized blocks have been refactored to use ReentrantLock instead.

- A virtual thread executor is now auto-configured for the RabbitMQ listener if virtual threads are enabled.

- A virtual thread executor is now auto-configured for the Kafka listener if virtual threads are enabled.

##### Servlet Web Servers

- When virtual threads are enabled, Tomcat and Jetty will use virtual threads for request processing. This means that your
application code that is handling a web request, such as a method in a controller, will run on a virtual thread.

##### Task Execution

- When virtual threads are enabled, the applicationTaskExectuor bean will be a SimpleAsyncTaskExecutor configured to use
  virtual threads. Anywhere that uses the application task executor, such as @EnableAsync when calling @Async methods,
  Spring MVC’s asynchronous request processing, and Spring WebFlux’s blocking execution support will now utilize virtual
  threads. As before, any TaskDecorator bean is applied to the auto-configured executor and the
  spring.task.execution.thread-name-prefix property is applied. Other spring.task.execution.* properties are ignored as
  they are specific to a pool-based executor.

- A SimpleAsyncTaskExecutorBuilder is now available in the application context and can be used to build a
  SimpleAsyncTaskExecutor. SimpleAsyncTaskExecutorCustomizer beans can be used to customize the built
  SimpleAsyncTaskExecutor. If virtual threads are enabled, the builder is auto-configured to use them.

#### Task Scheduling

- A SimpleAsyncTaskSchedulerBuilder is now available in the application context and can be used to build
  SimpleAsyncTaskScheduler. SimpleAsyncTaskSchedulerCustomizer beans can be used to customize the built
  SimpleAsyncTaskScheduler. If virtual threads are enabled, the builder is auto-configured to use them.

- When virtual threads are enabled, the taskScheduler bean will be a SimpleAsyncTaskScheduler configured to use virtual
  threads. The spring.task.scheduling.thread-name-prefix property and spring.task.scheduling.simple. properties are
  applied. Other spring.task.scheduling. properties are ignored as they are specific to a pool-based scheduler.

##### Blocking Execution with Spring WebFlux

- Spring WebFlux’s support for block execution is auto-configured to use the applicationTaskExecutor bean when it is an
AsyncTaskExecutor. The applicationTaskExecutor is an AsyncTaskExecutor both by default and when virtual threads are
enabled.

#### Link

- [Spring Boot 3.2 ships support for Virtual Threads in 3.2-M1 (Web/WebFlux, Task Execution)](https://github.com/spring-projects/spring-boot/wiki/Spring-Boot-3.2.0-M1-Release-Notes)
- [VT support update in 3.2-M2 (Lock refactor, RabbitMQ/Kafka, Task Scheduling)](https://github.com/spring-projects/spring-boot/wiki/Spring-Boot-3.2.0-M2-Release-Notes)
- [Spring Framework: General compatibility with virtual threads](https://github.com/spring-projects/spring-framework/wiki/What%27s-New-in-Spring-Framework-6.x)
