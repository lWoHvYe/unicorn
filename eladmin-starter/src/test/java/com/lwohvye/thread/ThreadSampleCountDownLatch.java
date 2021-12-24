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

package com.lwohvye.thread;

import cn.hutool.core.date.DateUtil;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.*;
import java.util.concurrent.*;

/**
 * 抽卡模拟
 * 将抽卡简化成随机取一个1000的样本中的数，取到指定的算抽中
 * 在取到需要的时，会将与其同样的从期望中一并移除
 * <p>
 * 使用多线程时，有时需关注其他线程的完成情况
 * 采用线程的方式  Thread
 */
// 该方式数据进行实时的共享，容易出现线程安全问题，但应用更广泛
// 但实际上只在最后交互了数据，无法达到预期的交互效果
// 这里使用了线程安全的ConcurrentHashMap替代HashTable，用于实时的交互，较HashTable效率高一些，但确保了线程的安全
//@SpringBootTest
public class ThreadSampleCountDownLatch {

    private Logger logger4j = LoggerFactory.getLogger(ThreadSampleCountDownLatch.class);
    //    用来存放模拟的结果，同步变量，使用线程安全的HashTable
    private volatile Map<String, Integer> countMap = new ConcurrentHashMap<>() {
        {
            put("s50", 0);
            put("s100", 0);
            put("s150", 0);
            put("s200", 0);
            put("s250", 0);
            put("s300", 0);
            put("s350", 0);
            put("s400", 0);
            put("s450", 0);
            put("s500", 0);
            put("other", 0);
        }
    };
    //   开启模拟线程数
    private static Integer threadCount = 10;
    //    用于控制主线程等待子线程结束
    private static final CountDownLatch latch = new CountDownLatch(threadCount);
    //        模拟次数
    private Integer simuCount = 1000000;


    public ThreadSampleCountDownLatch() {
    }

    @Test
    public void simuWork() {
        try {
//            记录开始时间
            long start = DateUtil.currentSeconds();
            ThreadSampleCountDownLatch threadSampleCountDownLatch = new ThreadSampleCountDownLatch();
//            开始模拟
            threadSampleCountDownLatch.startWork();
//            因为把结果输出放在主线程，所以需要设计主线程等待其他线程结束
            latch.await();
//            输出结果
            threadSampleCountDownLatch.printResult();
//            记录结束时间
            long end = DateUtil.currentSeconds();
//            输出模拟用时
            System.out.println(end - start);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    /**
     * 模拟入口
     */
    private void startWork() {
//        创建随机数
//        池子集合
        List<List<Integer>> lists = new ArrayList<>();
//        池子1 2% 2% 2.5% 2.5% 2.5%
        List<Integer> list1 = Arrays.asList(20, 20, 25, 25, 25);

//        池子2 2% 1.8% 1.8% 2.5% 5%
        List<Integer> list2 = Arrays.asList(20, 18, 25, 50);
//        将池子放入总集
        lists.add(list1);
        lists.add(list2);
//        开始模拟
        doWork(lists);
    }


    /**
     * 输出模拟结果
     */
    private void printResult() {
        System.out.println("输出结果");

        logger4j.info("50次以内：" + (double) countMap.get("s50") * 100 / simuCount + "%;");
        logger4j.info("100次以内：" + (double) countMap.get("s100") * 100 / simuCount + "%;");
        logger4j.info("150次以内：" + (double) countMap.get("s150") * 100 / simuCount + "%;");
        logger4j.info("200次以内：" + (double) countMap.get("s200") * 100 / simuCount + "%;");
        logger4j.info("250次以内：" + (double) countMap.get("s250") * 100 / simuCount + "%;");
        logger4j.info("300次以内：" + (double) countMap.get("s300") * 100 / simuCount + "%;");
        logger4j.info("350次以内：" + (double) countMap.get("s350") * 100 / simuCount + "%;");
        logger4j.info("400次以内：" + (double) countMap.get("s400") * 100 / simuCount + "%;");
        logger4j.info("450次以内：" + (double) countMap.get("s450") * 100 / simuCount + "%;");
        logger4j.info("500次以内：" + (double) countMap.get("s500") * 100 / simuCount + "%;");
        logger4j.info("500次以上：" + (double) countMap.get("other") * 100 / simuCount + "%;");
        System.out.println("总计模拟:" + (countMap.get("s50") + countMap.get("s100") + countMap.get("s150") + countMap.get("s200")
                                      + countMap.get("s250") + countMap.get("s300") + countMap.get("s350") + countMap.get("s400") + countMap.get("s450")
                                      + countMap.get("s500") + countMap.get("other")) + "次");
    }

    /**
     * 启动多线程
     *
     * @return void
     * @params [lists]
     * @author Hongyan Wang
     * @date 2019/9/24 13:19
     * @
     */
    private void doWork(List<List<Integer>> lists) {
//        使用线程池创建线程
//        var executor = Executors.newFixedThreadPool(6);
//        自定义线程池，初始6，最大8，线程活跃200s，队列最大20，默认消息超队列长度时抛异常
        ThreadPoolExecutor executor = new ThreadPoolExecutor(6, 8,
                200, TimeUnit.SECONDS, new ArrayBlockingQueue<>(20));
        try {
//        开启数个线程
            for (int i = 0; i < threadCount; i++) {
                SimuThread simuThread = new SimuThread(lists);
                executor.execute(simuThread);
            }
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } finally {
//        关闭线程池
            executor.shutdown();
            while (!executor.isTerminated()) ; // 这里可以等待一下结束。阻塞一下
        }
    }

    /**
     * 模拟多线程
     */
    class SimuThread implements Runnable {

        private List<List<Integer>> lists;
        private int s50 = 0;
        private int s100 = 0;
        private int s150 = 0;
        private int s200 = 0;
        private int s250 = 0;
        private int s300 = 0;
        private int s350 = 0;
        private int s400 = 0;
        private int s450 = 0;
        private int s500 = 0;
        private int other = 0;

        private Random random = SecureRandom.getInstanceStrong();

        private SimuThread(List<List<Integer>> lists) throws NoSuchAlgorithmException {
            this.lists = lists;
        }

        @Override
        public void run() {
            int singleCount = simuCount / threadCount;
            for (int j = 0; j < singleCount; j++) {
//            开始模拟
                int count = simulate(random, lists);
//                将模拟结果放入集合中
                countNumber(count);
            }
//              将模拟结果放入共享集合
            countMap.put("s50", countMap.get("s50") + s50);
            countMap.put("s100", countMap.get("s100") + s100);
            countMap.put("s150", countMap.get("s150") + s150);
            countMap.put("s200", countMap.get("s200") + s200);
            countMap.put("s250", countMap.get("s250") + s250);
            countMap.put("s300", countMap.get("s300") + s300);
            countMap.put("s350", countMap.get("s350") + s350);
            countMap.put("s400", countMap.get("s400") + s400);
            countMap.put("s450", countMap.get("s450") + s450);
            countMap.put("s500", countMap.get("s500") + s500);
            countMap.put("other", countMap.get("other") + other);
            System.out.println("运行结束");
            System.out.println(Thread.currentThread().getName());
//            线程执行结束，latch值减1
            latch.countDown();
        }

        /**
         * 模拟核心代码
         *
         * @return int
         * @params [random, lists]
         * @author Hongyan Wang
         * @date 2019/9/24 13:31
         */
        private int simulate(Random random, List<List<Integer>> lists) {
            //        抽卡数
            int count = 0;
//              存放目标集合，内部数个子集合
            List<List<Integer>> mblist = new ArrayList<>();
//            存放目标值的集合
            List<Integer> dblist = new ArrayList<>();
            for (List<Integer> list : lists) {
//            生成目标数值的开始值
                int start = 1;
                for (Integer integer : list) {
//                单个子集合
                    List<Integer> zlist = new ArrayList<>();
                    for (int i = 0; i < integer; i++) {
                        zlist.add(start);
                        start++;
                    }
                    mblist.add(zlist);
                    dblist.addAll(zlist);
                }
//            当目标值不为空时进行抽卡
                if (!dblist.isEmpty()) {
                    do {
//            开始抽卡
                        var result = random.nextInt(1000) + 1;
//                判断是否抽到目标卡
                        if (dblist.contains(result)) {
//                    当抽到目标卡时，遍历目标子集合
                            for (List<Integer> integerList : mblist) {
//                        判断目标是否在子集合中
                                if (integerList.contains(result)) {
//                            当目标在子集合中时，从目标集合中移除对应子集合内容
                                    dblist.removeAll(integerList);
                                }
                            }
                        }
//                抽卡次数+1
                        count++;
                    } while (!dblist.isEmpty());
                }
//            抽完一池，置空子集合
                mblist.clear();
            }
            return count;
        }

        /**
         * 根据单次模拟结果，确定区间
         *
         * @params count
         */
        private void countNumber(Integer count) {
            if (count <= 50) {
                s50++;
            } else if (count <= 100) {
                s100++;
            } else if (count <= 150) {
                s150++;
            } else if (count <= 200) {
                s200++;
            } else if (count <= 250) {
                s250++;
            } else if (count <= 300) {
                s300++;
            } else if (count <= 350) {
                s350++;
            } else if (count <= 400) {
                s400++;
            } else if (count <= 450) {
                s450++;
            } else if (count <= 500) {
                s500++;
            } else {
                other++;
            }
        }
    }
}
