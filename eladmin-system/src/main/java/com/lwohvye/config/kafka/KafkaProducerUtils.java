package com.lwohvye.config.kafka;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.support.SendResult;
import org.springframework.stereotype.Component;
import org.springframework.util.concurrent.ListenableFutureCallback;

import java.util.Objects;

@Slf4j
@Component
public class KafkaProducerUtils {
    @Autowired
    private KafkaTemplate<String, Object> kafkaTemplate;

    // 发送消息
    public void sendNormalMessage(String topic, Object normalMessage) {
        kafkaTemplate.send(topic, normalMessage);
    }

    //    带回调
    public void sendCallbackMessage(String topic, Object callbackMessage) {
        kafkaTemplate.send(topic, callbackMessage).addCallback(new ListenableFutureCallback<>() {
            @Override
            public void onFailure(Throwable throwable) {
                log.error("发送消息失败：" + throwable.getMessage());
            }

            @Override
            public void onSuccess(SendResult<String, Object> result) {
                var relTopic = result.getRecordMetadata().topic();
                var partition = result.getRecordMetadata().partition();
                var offset = result.getRecordMetadata().offset();
                log.info("发送消息成功:" + relTopic + "-" + partition + "-" + offset);
            }
        });
    }

    public void sendCallbackMessage2(String topic, Object callbackMessage) {
        kafkaTemplate.send(topic, callbackMessage).addCallback(
                success -> {
                    // 消息发送到的topic
                    assert success != null;
                    var recordMetadata = success.getRecordMetadata();
                    String relTopic = recordMetadata.topic();
                    // 消息发送到的分区
                    int partition = recordMetadata.partition();
                    // 消息在分区内的offset
                    long offset = recordMetadata.offset();
                    log.info("发送消息成功:" + relTopic + "-" + partition + "-" + offset);
                },
                failure -> {
                    log.error("发送消息失败:" + failure.getMessage());
                });
    }

    //---------------事务
    public void sendMessageWithTransaction(String topic,Object transactionMessage) {
        // 声明事务：后面报错消息不会发出去
        kafkaTemplate.executeInTransaction(operations -> {
            Objects.requireNonNull(operations).send(topic, transactionMessage).addCallback(new ListenableFutureCallback<>() {
                @Override
                public void onFailure(Throwable throwable) {
                    log.error("发送消息失败：" + throwable.getMessage());
                }

                @Override
                public void onSuccess(SendResult<String, Object> result) {
                    var relTopic = result.getRecordMetadata().topic();
                    var partition = result.getRecordMetadata().partition();
                    var offset = result.getRecordMetadata().offset();
                    log.info("发送消息成功:" + relTopic + "-" + partition + "-" + offset);
                }
            });
//            operations.send("topic1", "test executeInTransaction");
            // TODO: 2021/3/26 需要返回值
            throw new RuntimeException("fail");
//            return null;
        });

        // 不声明事务：后面报错但前面消息已经发送成功了
//        kafkaTemplate.send("topic1", "test executeInTransaction");
//        throw new RuntimeException("fail");
    }

}
