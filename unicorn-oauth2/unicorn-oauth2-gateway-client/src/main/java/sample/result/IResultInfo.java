/*
 *    Copyright (c) 2021-2024.  lWoHvYe(Hongyan Wang)
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
package sample.result;

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
    int getBusinessCode();

    List<T> getContent();

    String getDescription();

    long getTotalElements();
}
