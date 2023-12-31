/*
 *    Copyright (c) 2022-2024.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.sys.modules.infrastructure.logrecord.service;

import com.lwohvye.core.utils.json.JsonUtils;
import com.lwohvye.core.utils.rabbitmq.AmqpMsgEntity;
import com.lwohvye.sys.modules.rabbitmq.config.RabbitMQConfig;
import com.lwohvye.sys.modules.rabbitmq.service.RabbitMQProducerService;
import com.mzt.logapi.beans.LogRecord;
import com.mzt.logapi.service.ILogRecordService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Slf4j
@Service
@ConditionalOnClass(ILogRecordService.class)
@RequiredArgsConstructor
public class RabbitLogRecordService implements ILogRecordService {

    private final RabbitMQProducerService rabbitMQProducerService;

    @Override
    public void record(LogRecord logRecord) {
        var recordMsg = new AmqpMsgEntity().setMsgType("business").setMsgData(JsonUtils.toJSONString(logRecord)).setExtraData("saveMultiLog");
        //  发送消息
        rabbitMQProducerService.sendMsg(RabbitMQConfig.DIRECT_SYNC_EXCHANGE, RabbitMQConfig.BUSINESS_LOG_ROUTE_KEY, recordMsg);
    }

    @Override
    public List<LogRecord> queryLog(String bizNo, String type) {
        return Collections.emptyList();
    }

    @Override
    public List<LogRecord> queryLogByBizNo(String bizNo, String type, String subType) {
        return Collections.emptyList();
    }

}
