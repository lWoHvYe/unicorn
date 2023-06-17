- runBlocking and coroutineScope builders may look similar because they both wait for their body and all its children to
  complete. The main difference is that the runBlocking method blocks the current thread for waiting, while
  coroutineScope just suspends, releasing the underlying thread for other usages. Because of that difference,
  runBlocking is a regular function and coroutineScope is a suspending function.
- 翻译一下就是：runBlocking 是一个顶层函数，用于在主线程中阻塞执行协程代码。
  coroutineScope 是一个挂起函数，用于在协程中创建一个新的作用域，不会阻塞当前线程。
  runBlocking 阻塞当前线程，直到协程执行完毕。
  coroutineScope 挂起协程，等待所有子协程完成。
- async starts a new coroutine and returns a Deferred object. Deferred represents a concept known by other names such as
  Future or Promise. It stores a computation, but it defers the moment you get the final result; it promises the result
  sometime in the future.
  The main difference between async and launch is that launch is used to start a computation that isn't expected to
  return
  a specific result. launch returns a Job that represents the coroutine. It is possible to wait until it completes by
  calling Job.join().
