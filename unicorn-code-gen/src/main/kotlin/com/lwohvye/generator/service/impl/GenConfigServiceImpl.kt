/*
 *  Copyright 2019-2020 Zheng Jie
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.lwohvye.generator.service.impl

import com.lwohvye.generator.domain.GenConfig
import com.lwohvye.generator.repository.GenConfigRepository
import com.lwohvye.generator.service.IGenConfigService
import org.springframework.stereotype.Service
import java.io.File

/**
 * @author Zheng Jie
 * @date 2019-01-14
 */
@Service
class GenConfigServiceImpl(val genConfigRepository: GenConfigRepository) : IGenConfigService {
    override fun find(tableName: String?): GenConfig {
        return genConfigRepository.findByTableName(tableName) ?: GenConfig(tableName)
    }

    override fun update(tableName: String?, genConfig: GenConfig): GenConfig {
        val separator = File.separator
        val paths: Array<String>?
        val symbol = "\\"
        paths = if (symbol == separator) {
            genConfig.path?.split("\\\\".toRegex())?.dropLastWhile { it.isEmpty() }?.toTypedArray()
        } else {
            genConfig.path?.split(File.separator.toRegex())?.dropLastWhile { it.isEmpty() }?.toTypedArray()
        }
        val api = StringBuilder()
        for (path in paths!!) {
            api.append(path)
            api.append(separator)
            if ("src" == path) {
                api.append("api")
                break
            }
        }
        genConfig.apiPath = api.toString()
        return genConfigRepository.save(genConfig)
    }
}
