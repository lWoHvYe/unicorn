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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CompletableFuture;

/**
 * 抽卡模拟 将抽卡简化成随机取一个1000的样本中的数，取到指定的算抽中
 * 在取到需要的时，会将与其同样的从期望中一并移除
 * 由于模拟采用了随机数的方式，所以池子可以任意配置，不影响结果
 * 由于使用了多线程，所以需关注其他线程的完成情况
 * 采用Feature的方式，使用CompletableFuture的runAsync()构建没有返回的子线程，各子线程实时共享数据，使用synchronized同步代码块
 * 需尤其注意变量的作用范围问题
 *
 * @author Hongyan Wang
 * @packageName com.lwohvye.springboot.otherpart.common.local
 * @className FutureSampleSync
 * @date 2019/9/22 8:54
 */
// 使用CompletableFuture,子线程实时共享数据，使用synchronized同步代码块，不能使用volatile,因为volatile变量不能提供原子性，这样++动作线程不安全
//@SpringBootTest
public class FutureSampleSync {

    private Logger logger4j = LoggerFactory.getLogger(FutureSampleSync.class);

    private Integer s50 = 0;
    private Integer s100 = 0;
    private Integer s150 = 0;
    private Integer s200 = 0;
    private Integer s250 = 0;
    private Integer s300 = 0;
    private Integer s350 = 0;
    private Integer s400 = 0;
    private Integer s450 = 0;
    private Integer s500 = 0;
    private Integer other = 0;


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
    public void startWork() {
        //   开启模拟线程数
        int threadCount = 10;
//        模拟次数
        int simCount = 1000000;
//        记录开始时间
        long start = DateUtil.currentSeconds();
//        FutureSampleSync futureSample = new FutureSampleSync();
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
        CompletableFuture<Void>[] futuresArray = new CompletableFuture[threadCount];
//            开启模拟线程，使用线程池的方式创建CompletableFuture
        for (int i = 0; i < threadCount; i++) {
//            创建模拟线程，除了传执行方法，还可传线程池近去，不传会使用默认线程池ForkJoinPool.commonPool()
            CompletableFuture<Void> future = CompletableFuture.runAsync(simCallable);
//            将线程放入线程数组
            futuresArray[i] = future;
        }

//        设置需等待的子线程
        CompletableFuture<Void> result = CompletableFuture.allOf(futuresArray);
//        等待线程完成
        result.join();

//         输出总结果
//        futureSample.printResult(countMap, simCount / 100);
//         记录结束时间
        long end = DateUtil.currentSeconds();
//          输出结果
        logger4j.info("50次以内：" + (double) s50 * 100 / simCount + "%;");
        logger4j.info("100次以内：" + (double) s100 * 100 / simCount + "%;");
        logger4j.info("150次以内：" + (double) s150 * 100 / simCount + "%;");
        logger4j.info("200次以内：" + (double) s200 * 100 / simCount + "%;");
        logger4j.info("250次以内：" + (double) s250 * 100 / simCount + "%;");
        logger4j.info("300次以内：" + (double) s300 * 100 / simCount + "%;");
        logger4j.info("350次以内：" + (double) s350 * 100 / simCount + "%;");
        logger4j.info("400次以内：" + (double) s400 * 100 / simCount + "%;");
        logger4j.info("450次以内：" + (double) s450 * 100 / simCount + "%;");
        logger4j.info("500次以内：" + (double) s500 * 100 / simCount + "%;");
        logger4j.info("500次以上：" + (double) other * 100 / simCount + "%;");
        System.out.println("总计模拟:" + (s50 + s100 + s150 + s200 + s250 + s300 + s350 + s400 + s450 + s500 + other) + "次");

        System.out.println(end - start);
    }

    /**
     * 模拟多线程相关类
     *
     * @author Hongyan Wang
     * @className SimCallable
     * @date 2019/9/23 9:53
     */
    class SimCallable implements Runnable {
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
         *
         * @return void
         * @params []
         * @author Hongyan Wang
         * @date 2019/9/23 9:52
         */
        @Override
        public void run() {
            try {
                Random random = SecureRandom.getInstanceStrong();
//              生成乱序池子
                int[] ranArray = ranArray();
                for (int j = 0; j < simCount; j++) {
//                开始模拟
                    int count = simulateWork(random, lists, ranArray);
//                将模拟结果放入集合中
                    if (count <= 50) synchronized (s50) {
                        s50++;
                    }
                    else if (count <= 100) synchronized (s100) {
                        s100++;
                    }
                    else if (count <= 150) synchronized (s150) {
                        s150++;
                    }
                    else if (count <= 200) synchronized (s200) {
                        s200++;
                    }
                    else if (count <= 250) synchronized (s250) {
                        s250++;
                    }
                    else if (count <= 300) synchronized (s300) {
                        s300++;
                    }
                    else if (count <= 350) synchronized (s350) {
                        s350++;
                    }
                    else if (count <= 400) synchronized (s400) {
                        s400++;
                    }
                    else if (count <= 450) synchronized (s450) {
                        s450++;
                    }
                    else if (count <= 500) synchronized (s500) {
                        s500++;
                    }
                    else synchronized (other) {
                            other++;
                        }
                }
                System.out.println(Thread.currentThread().getName());
            } catch (NoSuchAlgorithmException e) {
                logger4j.info(e.getMessage());
            }
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
            var ranArrays = new int[1000];
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
