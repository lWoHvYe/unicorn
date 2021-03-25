package com.lwohvye.modules.content.rest;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.map.MapUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ObjectUtil;
import com.lwohvye.domain.Log;
import com.lwohvye.repository.LogRepository;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.annotation.PartitionOffset;
import org.springframework.kafka.annotation.TopicPartition;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.listener.ConsumerAwareListenerErrorHandler;
import org.springframework.messaging.handler.annotation.SendTo;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.List;
import java.util.Map;

@Component
public class KafkaConsumer {
    // 消费监听
    @KafkaListener(topics = {"topic1"})
    public void onMessage1(ConsumerRecord<?, ?> record) {
        // 消费的哪个topic、partition的消息,打印出消息内容
        System.out.println("简单消费：" + record.topic() + "-" + record.partition() + "-" + record.value());
    }

//    指定消费对象

    /**
     * @return void
     * @Title 指定topic、partition、offset消费
     * @Description 同时监听topic1和topic2，监听topic1的0号分区、topic2的 "0号和1号" 分区，指向1号分区的offset初始值为8
     * 属性解释：
     * ① id：消费者ID；
     * ② groupId：消费组ID；
     * ③ topics：监听的topic，可监听多个；
     * ④ topicPartitions：可配置更加详细的监听信息，可指定topic、parition、offset监听。
     * 上面onMessage2监听的含义：监听topic1的0号分区，同时监听topic2的0号分区和topic2的1号分区里面offset从8开始的消息。
     * 注意：topics和topicPartitions不能同时使用；
     * @Param [record]
     **/
    @KafkaListener(id = "consumer1", groupId = "felix-group", topicPartitions = {
            @TopicPartition(topic = "topic1", partitions = {"0"}),
            @TopicPartition(topic = "topic2", partitions = "0", partitionOffsets = @PartitionOffset(partition = "1", initialOffset = "8"))
    }, errorHandler = "consumerAwareErrorHandler")
    public void onMessage2(ConsumerRecord<?, ?> record) {
        System.out.println("topic:" + record.topic() + "|partition:" + record.partition() + "|offset:" + record.offset() + "|value:" + record.value());
    }

    //    ------批量消费
    @KafkaListener(id = "consumer2", groupId = "felix-group", topics = "topic1", errorHandler = "consumerAwareErrorHandler")
    public void onMessage3(List<ConsumerRecord<?, ?>> records) {
        System.out.println(">>>批量消费一次，records.size()=" + records.size());
        for (ConsumerRecord<?, ?> record : records) {
            System.out.println("批量消费内容：" + record.value());
        }
    }

    //-----------统一异常处理 ConsumerAwareListenerErrorHandler 异常处理器
    // 新建一个异常处理器，用@Bean注入
    @Bean
    public ConsumerAwareListenerErrorHandler consumerAwareErrorHandler() {
        return (message, exception, consumer) -> {
            System.out.println("消费异常：" + message.getPayload());
            return message.getPayload();
        };
    }

    // 将这个异常处理器的BeanName放到@KafkaListener注解的errorHandler属性里面
//    @KafkaListener(topics = {"topic1"}, errorHandler = "consumerAwareErrorHandler")
//    public void onMessage4(ConsumerRecord<?, ?> record) throws Exception {
//        throw new Exception("简单消费-模拟异常");
//    }

    // 批量消费也一样，异常处理器的message.getPayload()也可以拿到各条消息的信息
//    @KafkaListener(topics = "topic1", errorHandler = "consumerAwareErrorHandler")
//    public void onMessage5(List<ConsumerRecord<?, ?>> records) throws Exception {
//        System.out.println("批量消费一次...");
//        throw new Exception("批量消费-模拟异常");
//    }

    //------消息过滤
    @Autowired
    private ConsumerFactory consumerFactory;

    // 消息过滤器
    @Bean
    public ConcurrentKafkaListenerContainerFactory filterContainerFactory() {
        ConcurrentKafkaListenerContainerFactory factory = new ConcurrentKafkaListenerContainerFactory();
        factory.setConsumerFactory(consumerFactory);
        // 被过滤的消息将被丢弃
        factory.setAckDiscarded(true);
        // 消息过滤策略
        factory.setRecordFilterStrategy(consumerRecord -> {
//            根据具体需求设置
            //返回true消息则被过滤
            var value = consumerRecord.value();
            if (value instanceof Collection) {
                return CollUtil.isEmpty((Iterable<?>) value);
            } else if (value instanceof Map) {
                return MapUtil.isEmpty((Map<?, ?>) value);
            } else if (value instanceof String) {
                return CharSequenceUtil.isEmpty((CharSequence) value);
            } else {
                return ObjectUtil.isEmpty(value);
            }
        });
        return factory;
    }

    // 消息过滤监听
    @KafkaListener(topics = {"topic1"}, containerFactory = "filterContainerFactory")
    public void onMessage6(ConsumerRecord<?, ?> record) {
        System.out.println("过滤监听：" + record.value());
    }

//    ------消息转发

    /**
     * @return void
     * @Title 消息转发。使用@SendTo注解
     * @Description 从topic1接收到的消息经过处理后转发到topic2
     * @Param [record]
     **/
    @KafkaListener(topics = {"topic1"})
    @SendTo("topic2")
    public String onMessage7(ConsumerRecord<?, ?> record) {
        return record.value() + "-forward message";
    }

    //    -------------------记录鉴权信息-----------------------------

    @Autowired
    private LogRepository logRepository;

    @KafkaListener(id = "consumer4", groupId = "felix-group", topics = "auth", errorHandler = "consumerAwareErrorHandler")
    public void saveAuthorizeLog(List<ConsumerRecord<?, ?>> records) {
        for (ConsumerRecord<?, ?> record : records) {
            var log = new Log().setDescription("记录用户登录信息").setLogType("Auth").setParams(record.toString());
            logRepository.save(log);
        }
    }
}
