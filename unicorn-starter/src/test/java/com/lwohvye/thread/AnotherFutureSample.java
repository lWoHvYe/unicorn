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

package com.lwohvye.thread;

import cn.hutool.core.date.DateUtil;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

/**
 * 抽卡模拟 将抽卡简化成随机取一个1000的样本中的数，取到指定的算抽中
 * 在取到需要的时，会将与其同样的从期望中一并移除
 * 由于模拟采用了随机数的方式，所以池子可以任意配置，不影响结果
 * 由于使用了多线程，所以需关注其他线程的完成情况
 * 采用Feature的方式，使用CompletableFuture的supplyAsync()构建子线程，并获取返回结果进行处理
 * 经过调整使用ThreadLocal修饰变量，简化线程内各函数的传值，但会一定程度上降低效率
 * 需尤其注意变量的作用范围问题
 *
 * @author Hongyan Wang
 * @packageName com.lwohvye.springboot.otherpart.common.local
 * @className AnotherFutureSample
 * @date 2019/9/22 8:54
 */
// 使用CompletableFuture，开启的线程数受CPU支持的线程数影响较大，通过更改线程数，发现执行时间方差较大
// 该方式并没有线程间的共享数据，所以不会出现线程安全问题，但可能有一定的局限性
// 设置传入概率及池子的方法
//TODO 优化统计中的if else
//@SpringBootTest
public class AnotherFutureSample {

    private Logger logger4j = LoggerFactory.getLogger(AnotherFutureSample.class);

    private ThreadLocal<Integer> s50 = ThreadLocal.withInitial(() -> 0);
    private ThreadLocal<Integer> s100 = ThreadLocal.withInitial(() -> 0);
    private ThreadLocal<Integer> s150 = ThreadLocal.withInitial(() -> 0);
    private ThreadLocal<Integer> s200 = ThreadLocal.withInitial(() -> 0);
    private ThreadLocal<Integer> s250 = ThreadLocal.withInitial(() -> 0);
    private ThreadLocal<Integer> s300 = ThreadLocal.withInitial(() -> 0);
    private ThreadLocal<Integer> s350 = ThreadLocal.withInitial(() -> 0);
    private ThreadLocal<Integer> s400 = ThreadLocal.withInitial(() -> 0);
    private ThreadLocal<Integer> s450 = ThreadLocal.withInitial(() -> 0);
    private ThreadLocal<Integer> s500 = ThreadLocal.withInitial(() -> 0);
    private ThreadLocal<Integer> other = ThreadLocal.withInitial(() -> 0);

    //    Map的key和value
    private String[] keys = new String[]{"s50", "s100", "s150", "s200", "s250", "s300", "s350", "s400", "s450", "s500", "other"};
    private ThreadLocal[] values = new ThreadLocal[]{s50, s100, s150, s200, s250, s300, s350, s400, s450, s500, other};


    /**
     * 方法主体，用于模拟调用，获取及输出模拟结果
     *
     * @return void
     * @params []
     * @author Hongyan Wang
     * @date 2019/9/23 9:59
     */
    @Test
    @SuppressWarnings("unchecked")
    public void startWork() throws ExecutionException, InterruptedException {
//        存放总结果集
        Map<String, Integer> countMap = new HashMap<>();
//        创建存放子线程返回结果的List
        List<Map<String, Integer>> resultList = new ArrayList<>();
        //   开启模拟线程数
        int threadCount = 10;
//        模拟次数
        int simCount = 1000000;
//        记录开始时间
        long start = DateUtil.currentSeconds();
//        创建随机数
//        池子集合
        List<List<Integer>> lists = new ArrayList<>();
//        池子1 2% 2% 2.5% 2.5% 2.5%
        List<Integer> list1 = Arrays.asList(20, 20, 25, 25, 25);

//        池子2 2% 1.8% 1.8% 2.5% 5%
        List<Integer> list2 = Arrays.asList(20, 18, 18, 25, 50);
//        池子3 2% 2% 2.5% 5%
//        List<Integer> list3 = Arrays.asList(20, 20, 25, 50);
//        可以根据需求调整池子，将概率乘以1000即为预放入集合中的值，之后需要把池子放入总集
        lists.add(list1);
        lists.add(list2);
//        lists.add(list3);
//        设置模拟池子
        SimCallable simCallable = new SimCallable(lists, simCount / threadCount);
//        创建线程数组
        CompletableFuture<Map<String, Integer>>[] futuresArray = new CompletableFuture[threadCount];
//            开启模拟线程，使用线程池的方式创建CompletableFuture
        for (int i = 0; i < threadCount; i++) {
//            创建模拟线程，除了传执行方法，还可传线程池近去，不传会使用默认线程池ForkJoinPool.commonPool()
            CompletableFuture<Map<String, Integer>> future = CompletableFuture.supplyAsync(simCallable::call);
//            将线程放入线程数组
            futuresArray[i] = future;
        }

//        设置需等待的子线程
        CompletableFuture<Void> result = CompletableFuture.allOf(futuresArray);
//        等待线程完成
        result.join();

//        获取各子线程模拟结果
        for (int j = 0; j < threadCount; j++) {
            resultList.add(futuresArray[j].get());
        }

//        总模拟次数
        int totalCount = 0;
        for (String key : keys) {
            int value = 0;
            for (Map<String, Integer> integerMap : resultList) {
                value += integerMap.get(key);
            }

            countMap.put(key, value);
            totalCount += value;
        }
        Arrays.asList(values).forEach(ThreadLocal::remove); // 移除，避免内存泄漏

//         输出总结果
        this.printResult(countMap, simCount / 100, totalCount);
//         记录结束时间
        long end = DateUtil.currentSeconds();
        System.out.println(end - start);

    }

    /**
     * 输出模拟结果
     *
     * @return void
     * @params [countMap, simCount, totalCount]
     * @author Hongyan Wang
     * @date 2019/9/24 13:56
     */
    private void printResult(Map<String, Integer> countMap, Integer simCount, Integer totalCount) {

        System.out.println("输出结果");

        logger4j.info(String.format("50次以内：%s%%;", (double) countMap.get("s50") / simCount));
        logger4j.info(String.format("100次以内：%s%%;", (double) countMap.get("s100") / simCount));
        logger4j.info(String.format("150次以内：%s%%;", (double) countMap.get("s150") / simCount));
        logger4j.info(String.format("200次以内：%s%%;", (double) countMap.get("s200") / simCount));
        logger4j.info(String.format("250次以内：%s%%;", (double) countMap.get("s250") / simCount));
        logger4j.info(String.format("300次以内：%s%%;", (double) countMap.get("s300") / simCount));
        logger4j.info(String.format("350次以内：%s%%;", (double) countMap.get("s350") / simCount));
        logger4j.info(String.format("400次以内：%s%%;", (double) countMap.get("s400") / simCount));
        logger4j.info(String.format("450次以内：%s%%;", (double) countMap.get("s450") / simCount));
        logger4j.info(String.format("500次以内：%s%%;", (double) countMap.get("s500") / simCount));
        logger4j.info(String.format("500次以上：%s%%;", (double) countMap.get("other") / simCount));

        switch (simCount % (totalCount / 100)) {
            case 0:
                logger4j.info(String.format("总计模拟:%d次", totalCount));
                break;
            case 1:
                logger4j.info("出现数据丢失，请核查原因");
                break;
            default:
                logger4j.info("系统出错，请重试");
        }
    }

    /**
     * 模拟多线程相关类
     *
     * @author Hongyan Wang
     * @className SimCallable
     * @date 2019/9/23 9:53
     */
    class SimCallable implements Callable<Map<String, Integer>> {
        //        卡池集
        private List<List<Integer>> lists;
        //        总模拟次数
        private Integer simCount;

        private SimCallable(List<List<Integer>> lists, Integer simCount) {
            this.lists = lists;
            this.simCount = simCount;
        }

        /**
         * 不再使用同步变量，直接将各子线程结果返回，由主线程处理,
         * 池子是否乱序并不影响结果，若每次模拟都重新生成乱序池子将大幅降低效率，可以一个线程只使用一个乱序池子，但实际意义不大
         * Computes a result, or throws an exception if unable to do so.
         *
         * @return computed result
         * @throws Exception if unable to compute a result
         * @params []
         * @author Hongyan Wang
         * @date 2019/9/23 9:52
         */
        @Override
        public Map<String, Integer> call() {
//          记录本线程模拟结果集
            Map<String, Integer> countHashMap = new HashMap<>();
            try {
                Random random = SecureRandom.getInstanceStrong(); // 线程安全的
                // var ri = ThreadLocalRandom.current().nextInt(); // 也可以获取随机数。较上面这种可能更好一些
//              生成乱序池子
                int[] ranArray = ranArray();
                for (int j = 0; j < simCount; j++) {
//                开始模拟
                    int count = simulateWork(random, lists, ranArray);
                    //TODO 后续需对统计进行优化
//                将模拟结果放入集合中
                    if (count <= 50) {
                        Integer integer50 = s50.get();
                        s50.set(integer50 + 1);
                    } else if (count <= 100) {
                        Integer integer100 = s100.get();
                        s100.set(integer100 + 1);
                    } else if (count <= 150) {
                        Integer integer150 = s150.get();
                        s150.set(integer150 + 1);
                    } else if (count <= 200) {
                        Integer integer200 = s200.get();
                        s200.set(integer200 + 1);
                    } else if (count <= 250) {
                        Integer integer250 = s250.get();
                        s250.set(integer250 + 1);
                    } else if (count <= 300) {
                        Integer integer300 = s300.get();
                        s300.set(integer300 + 1);
                    } else if (count <= 350) {
                        Integer integer350 = s350.get();
                        s350.set(integer350 + 1);
                    } else if (count <= 400) {
                        Integer integer400 = s400.get();
                        s400.set(integer400 + 1);
                    } else if (count <= 450) {
                        Integer integer450 = s450.get();
                        s450.set(integer450 + 1);
                    } else if (count <= 500) {
                        Integer integer500 = s500.get();
                        s500.set(integer500 + 1);
                    } else {
                        Integer integer0 = other.get();
                        other.set(integer0 + 1);
                    }

                }
            } catch (NoSuchAlgorithmException e) {
                logger4j.info(e.getMessage());
            }

            for (int i = 0; i < keys.length; i++) {
//            将模拟结果放入集合返回
                countHashMap.put(keys[i], (Integer) values[i].get());
//            执行结束，移除所属的ThreadLocal变量，防止出现内存问题
                values[i].remove();
//                logger4j.info(Thread.currentThread().getName() + " : " + keys[i] + " : " + values[i].get());
            }


            System.out.println("运行结束");
            System.out.println(Thread.currentThread().getName());
            return countHashMap;
        }

        /**
         * 生成乱序不重复数组，作为模拟池
         *
         * @return int[]
         * @params []
         * @author Hongyan Wang
         * @date 2019/9/23 9:51
         */
        private int[] ranArray() {
            int[] ranArrays = new int[1000];
            try {
                for (int i = 0; i < 1000; i++) ranArrays[i] = i + 1;
                Random r = SecureRandom.getInstanceStrong();
                for (int i = 0; i < 1000; i++) {
                    int in = r.nextInt(1000 - i) + i;
                    int t = ranArrays[in];
                    ranArrays[in] = ranArrays[i];
                    ranArrays[i] = t;
                }
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
            }
            return ranArrays;
        }

        /**
         * 核心代码
         * 模拟抽卡，当前为单个池子，根据要求，生成数个不重复的数值集合，
         * 当结果在某个数值集合中时，从目标集合中移除其所在的集合
         * 当前使用连续生成数值的方式
         *
         * @return int
         * @params [random, lists, ranArray]
         * @author Hongyan Wang
         * @date 2019/9/23 9:39
         */
        private int simulateWork(Random random, List<List<Integer>> lists, int[] ranArray) {
            //        抽卡数
            int count = 0;
//                存放单个池子目标集合，内部数个子集合
            List<List<Integer>> multiList = new ArrayList<>();
//                存放单个池子目标值的集合，主要为了避免每次模拟都要对multiList进行两次遍历
            List<Integer> numList = new ArrayList<>();
//            对池子进行模拟，抽完一个之后，再抽下一个
            for (List<Integer> list : lists) {
//                  生成目标数值的开始值
                int index = 1;
                for (Integer integer : list) {
//                      单个子集合
                    List<Integer> singleList = new ArrayList<>();
                    for (int i = 0; i < integer; i++) {
                        singleList.add(ranArray[index]);
                        index++;
                    }
                    numList.addAll(singleList);
                    multiList.add(singleList);
                }
                //TODO 模拟部分是花费时间最多的地方，是主要的优化部分
//            当目标值不为空时进行抽卡
                while (!numList.isEmpty()) {
//            开始抽卡
                    var result = random.nextInt(1000) + 1;
//                判断是否抽到目标卡
                    if (numList.contains(result))
//                    当抽到目标卡时，遍历目标子集合
                        for (List<Integer> integerList : multiList)
//                        判断目标是否在子集合中
                            if (integerList.contains(result))
//                            当目标在子集合中时，从目标集合中移除对应子集合内容
                                numList.removeAll(integerList);
//                抽卡次数+1
                    count++;
                }
//            抽完一池，置空子集合
                multiList.clear();
            }
            return count;
        }
    }
}
