package com.lwohvye.utils.result;

import java.io.Serializable;
import java.util.List;

/**
 * @author lvjian
 * @Title:
 * @Package
 * @Description:
 * @date 2021/1/7 17:26
 */
public interface IResultInfo<T> extends Serializable {
    long getBusinessCode();

    List<T> getContent();

    String getDescription();

    long getTotalElements();
}
