package com.lwohvye.config.rabbitmq;

import lombok.Getter;

@Getter
public enum DataOperatorEnum {

    CREATE("create"),

    UPDATE("update"),

    DELETE("delete");

    private final String value;

    DataOperatorEnum(String value) {
        this.value = value;
    }
}
