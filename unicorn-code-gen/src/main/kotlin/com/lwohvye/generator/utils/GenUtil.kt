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
package com.lwohvye.generator.utils

import cn.hutool.core.util.StrUtil
import cn.hutool.extra.template.Template
import cn.hutool.extra.template.TemplateConfig
import cn.hutool.extra.template.TemplateException
import cn.hutool.extra.template.TemplateUtil
import com.lwohvye.core.exception.UtilsException
import com.lwohvye.core.utils.FileUtils
import com.lwohvye.core.utils.StringUtils
import com.lwohvye.generator.domain.ColumnInfo
import com.lwohvye.generator.domain.GenConfig
import lombok.extern.slf4j.Slf4j
import org.springframework.util.ObjectUtils
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.io.Writer
import java.time.LocalDate

/**
 * 代码生成
 *
 * @author Zheng Jie
 * @date 2019-01-02
 */
object GenUtil {
    private const val TIMESTAMP = "Timestamp"
    private const val BIGDECIMAL = "BigDecimal"
    const val PK = "PRI"
    const val EXTRA = "auto_increment"
    private val adminTemplateNames: List<String>
        /**
         * 获取后端代码模板名称。根据需要调整要生成的类
         *
         * @return List
         */
        get() {
            val templateNames: MutableList<String> = ArrayList()
            templateNames.add("Entity")
            templateNames.add("Dto")
            templateNames.add("Mapper")
            templateNames.add("Controller")
            templateNames.add("QueryCriteria")
            templateNames.add("IService")
            templateNames.add("ServiceImpl")
            templateNames.add("Repository")
            return templateNames
        }
    private val frontTemplateNames: List<String>
        /**
         * 获取前端代码模板名称
         *
         * @return List
         */
        get() {
            val templateNames: MutableList<String> = ArrayList()
            templateNames.add("index")
            templateNames.add("api")
            return templateNames
        }

    fun preview(columns: List<ColumnInfo?>?, genConfig: GenConfig?): List<Map<String, Any>> {
        val genMap = getGenMap(columns, genConfig)
        val genList: MutableList<Map<String, Any>> = ArrayList()
        // 获取后端模版
        var templates = adminTemplateNames
        val engine = TemplateUtil.createEngine(TemplateConfig("template", TemplateConfig.ResourceMode.CLASSPATH))
        for (templateName in templates) {
            val map: MutableMap<String, Any> = HashMap(1)
            val template = engine.getTemplate("generator/admin/$templateName.ftl")
            map["content"] = template.render(genMap)
            map["name"] = templateName
            genList.add(map)
        }
        // 获取前端模版
        templates = frontTemplateNames
        for (templateName in templates) {
            val map: MutableMap<String, Any> = HashMap(1)
            val template = engine.getTemplate("generator/front/$templateName.ftl")
            map[templateName] = template.render(genMap)
            map["content"] = template.render(genMap)
            map["name"] = templateName
            genList.add(map)
        }
        return genList
    }

    @Throws(IOException::class)
    fun download(columns: List<ColumnInfo?>?, genConfig: GenConfig?): String {
        // 拼接的路径：/tmpeladmin-gen-temp/，这个路径在Linux下需要root用户才有权限创建,非root用户会权限错误而失败，更改为： /tmp/eladmin-gen-temp/
        // String tempPath =SYS_TEM_DIR + "eladmin-gen-temp" + File.separator + genConfig.getTableName() + File.separator;
        val tempPath =
            FileUtils.SYS_TEM_DIR + "eladmin-gen-temp" + File.separator + genConfig!!.tableName + File.separator
        val genMap = getGenMap(columns, genConfig)
        val engine = TemplateUtil.createEngine(TemplateConfig("template", TemplateConfig.ResourceMode.CLASSPATH))
        // 生成后端代码
        var templates = adminTemplateNames
        for (templateName in templates) {
            val template = engine.getTemplate("generator/admin/$templateName.ftl")
            val filePath = getAdminFilePath(
                templateName,
                genConfig,
                genMap["className"].toString(),
                tempPath + "eladmin" + File.separator
            )!!
            val file = File(filePath)
            // 如果非覆盖生成
            if (!genConfig.cover && FileUtils.exist(file)) {
                continue
            }
            // 生成代码
            genFile(file, template, genMap)
        }
        // 生成前端代码
        templates = frontTemplateNames
        for (templateName in templates) {
            val template = engine.getTemplate("generator/front/$templateName.ftl")
            val path = tempPath + "eladmin-web" + File.separator
            val apiPath = path + "src" + File.separator + "api" + File.separator
            val srcPath =
                path + "src" + File.separator + "views" + File.separator + genMap["changeClassName"].toString() + File.separator
            val filePath = getFrontFilePath(templateName, apiPath, srcPath, genMap["changeClassName"].toString())!!
            val file = File(filePath)
            // 如果非覆盖生成
            if (!genConfig.cover && FileUtils.exist(file)) {
                continue
            }
            // 生成代码
            genFile(file, template, genMap)
        }
        return tempPath
    }

    @Throws(IOException::class)
    fun generatorCode(columnInfos: List<ColumnInfo?>?, genConfig: GenConfig?) {
        val genMap = getGenMap(columnInfos, genConfig)
        val engine = TemplateUtil.createEngine(TemplateConfig("template", TemplateConfig.ResourceMode.CLASSPATH))
        // 生成后端代码
        var templates = adminTemplateNames
        for (templateName in templates) {
            val template = engine.getTemplate("generator/admin/$templateName.ftl")
            val rootPath = System.getProperty("user.dir")
            val filePath = getAdminFilePath(templateName, genConfig, genMap["className"].toString(), rootPath)!!
            val file = File(filePath)

            // 如果非覆盖生成
            if (!genConfig!!.cover && FileUtils.exist(file)) {
                continue
            }
            // 生成代码
            genFile(file, template, genMap)
        }

        // 生成前端代码。前端路径无值时不生成
        if (StrUtil.isNotBlank(genConfig!!.path)) {
            templates = frontTemplateNames
            for (templateName in templates) {
                val template = engine.getTemplate("generator/front/$templateName.ftl")
                val filePath = genConfig.apiPath?.let {
                    genConfig.path?.let { path ->
                        getFrontFilePath(
                            templateName,
                            it,
                            path,
                            genMap["changeClassName"].toString()
                        )
                    }
                }!!
                val file = File(filePath)

                // 如果非覆盖生成
                if (!genConfig.cover && FileUtils.exist(file)) {
                    continue
                }
                // 生成代码
                genFile(file, template, genMap)
            }
        }
    }

    // 获取模版数据
    private fun getGenMap(columnInfos: List<ColumnInfo?>?, genConfig: GenConfig?): Map<String?, Any?> {
        // 存储模版字段数据
        val genMap: MutableMap<String?, Any?> = HashMap(16)
        // 接口别名
        genMap["apiAlias"] = genConfig!!.apiAlias
        // 包名称
        genMap["package"] = genConfig.pack
        // 模块名称
        genMap["moduleName"] = genConfig.moduleName
        // 作者
        genMap["author"] = genConfig.author
        // 创建日期
        genMap["date"] = LocalDate.now().toString()
        // 表名
        genMap["tableName"] = genConfig.tableName
        // 大写开头的类名
        var className = StringUtils.toCapitalizeCamelCase(genConfig.tableName)
        // 小写开头的类名
        var changeClassName = StringUtils.toCamelCase(genConfig.tableName)
        // 判断是否去除表前缀
        if (StringUtils.isNotEmpty(genConfig.prefix)) {
            className =
                StringUtils.toCapitalizeCamelCase(StrUtil.removePrefix(genConfig.tableName, genConfig.prefix))
            changeClassName =
                StringUtils.toCamelCase(StrUtil.removePrefix(genConfig.tableName, genConfig.prefix))
            changeClassName = StringUtils.uncapitalize(changeClassName)
        }
        // 保存类名
        genMap["className"] = className
        // 保存小写开头的类名
        genMap["changeClassName"] = changeClassName
        // 存在 Timestamp 字段
        genMap["hasTimestamp"] = false
        // 查询类中存在 Timestamp 字段
        genMap["queryHasTimestamp"] = false
        // 存在 BigDecimal 字段
        genMap["hasBigDecimal"] = false
        // 查询类中存在 BigDecimal 字段
        genMap["queryHasBigDecimal"] = false
        // 是否需要创建查询
        genMap["hasQuery"] = false
        // 自增主键
        genMap["auto"] = false
        // 存在字典
        genMap["hasDict"] = false
        // 存在日期注解
        genMap["hasDateAnnotation"] = false
        // 保存字段信息
        val columns: MutableList<Map<String, Any?>> = ArrayList()
        // 保存查询字段的信息
        val queryColumns: MutableList<Map<String, Any?>> = ArrayList()
        // 存储字典信息
        val dicts: MutableList<String> = ArrayList()
        // 存储 between 信息
        val betweens: MutableList<Map<String, Any?>> = ArrayList()
        // 存储不为空的字段信息
        val isNotNullColumns: MutableList<Map<String, Any?>> = ArrayList()
        for (column in columnInfos!!) {
            val listMap: MutableMap<String, Any?> = HashMap(16)
            // 字段描述
            listMap["remark"] = column!!.remark
            // 字段类型
            listMap["columnKey"] = column.keyType
            // 主键类型
            val colType = ColUtil.cloToJava(column.columnType)
            // 小写开头的字段名
            val changeColumnName = StringUtils.toCamelCase(column.columnName)
            // 大写开头的字段名
            val capitalColumnName = StringUtils.toCapitalizeCamelCase(column.columnName)
            if (PK == column.keyType) {
                // 存储主键类型
                genMap["pkColumnType"] = colType
                // 存储小写开头的字段名
                genMap["pkChangeColName"] = changeColumnName
                // 存储大写开头的字段名
                genMap["pkCapitalColName"] = capitalColumnName
            }
            // 是否存在 Timestamp 类型的字段
            if (TIMESTAMP == colType) {
                genMap["hasTimestamp"] = true
            }
            // 是否存在 BigDecimal 类型的字段
            if (BIGDECIMAL == colType) {
                genMap["hasBigDecimal"] = true
            }
            // 主键是否自增
            if (EXTRA == column.extra) {
                genMap["auto"] = true
            }
            // 主键存在字典
            if (StringUtils.isNotBlank(column.dictName)) {
                genMap["hasDict"] = true
                if (!dicts.contains(column.dictName)) column.dictName?.let { dicts.add(it) }
            }

            // 存储字段类型
            listMap["columnType"] = colType
            // 存储字原始段名称
            listMap["columnName"] = column.columnName
            // 不为空
            listMap["istNotNull"] = column.notNull
            // 字段列表显示
            listMap["columnShow"] = column.listShow
            // 表单显示
            listMap["formShow"] = column.formShow
            // 表单组件类型
            listMap["formType"] = if (StringUtils.isNotBlank(column.formType)) column.formType else "Input"
            // 小写开头的字段名称
            listMap["changeColumnName"] = changeColumnName
            //大写开头的字段名称
            listMap["capitalColumnName"] = capitalColumnName
            // 字典名称
            listMap["dictName"] = column.dictName
            // 日期注解
            listMap["dateAnnotation"] = column.dateAnnotation
            if (StringUtils.isNotBlank(column.dateAnnotation)) {
                genMap["hasDateAnnotation"] = true
            }
            // 添加非空字段信息
            if (column.notNull) {
                isNotNullColumns.add(listMap)
            }
            // 判断是否有查询，如有则把查询的字段set进columnQuery
            if (!StringUtils.isBlank(column.queryType)) {
                // 查询类型
                listMap["queryType"] = column.queryType
                // 是否存在查询
                genMap["hasQuery"] = true
                if (TIMESTAMP == colType) {
                    // 查询中存储 Timestamp 类型
                    genMap["queryHasTimestamp"] = true
                }
                if (BIGDECIMAL == colType) {
                    // 查询中存储 BigDecimal 类型
                    genMap["queryHasBigDecimal"] = true
                }
                if ("between".equals(column.queryType, ignoreCase = true)) {
                    betweens.add(listMap)
                } else {
                    // 添加到查询列表中
                    queryColumns.add(listMap)
                }
            }
            // 添加到字段列表中
            columns.add(listMap)
        }
        // 保存字段列表
        genMap["columns"] = columns
        // 保存查询列表
        genMap["queryColumns"] = queryColumns
        // 保存字段列表
        genMap["dicts"] = dicts
        // 保存查询列表
        genMap["betweens"] = betweens
        // 保存非空字段信息
        genMap["isNotNullColumns"] = isNotNullColumns
        return genMap
    }

    /**
     * 定义后端文件路径以及名称
     */
    private fun getAdminFilePath(
        templateName: String,
        genConfig: GenConfig?,
        className: String,
        rootPath: String
    ): String? {
        // 若不设置模块名称，则不生成在模块内
        val moduleName = genConfig!!.moduleName
        val projectPath = if (StringUtils.isNotBlank(moduleName)) rootPath + File.separator + moduleName else rootPath
        var packagePath =
            projectPath + File.separator + "src" + File.separator + "main" + File.separator + "java" + File.separator
        if (!ObjectUtils.isEmpty(genConfig.pack)) {
            packagePath += genConfig.pack!!.replace(".", File.separator) + File.separator
        }
        if ("Entity" == templateName) {
            return packagePath + "domain" + File.separator + className + ".java"
        }
        if ("Controller" == templateName) {
            return packagePath + "rest" + File.separator + className + "Controller.java"
        }
        if ("IService" == templateName) {
            return packagePath + "service" + File.separator + "I" + className + "Service.java"
        }
        if ("ServiceImpl" == templateName) {
            return packagePath + "service" + File.separator + "impl" + File.separator + className + "ServiceImpl.java"
        }
        if ("Dto" == templateName) {
            return packagePath + "service" + File.separator + "dto" + File.separator + className + "Dto.java"
        }
        if ("QueryCriteria" == templateName) {
            return packagePath + "service" + File.separator + "dto" + File.separator + className + "QueryCriteria.java"
        }
        if ("Mapper" == templateName) {
            return packagePath + "service" + File.separator + "mapstruct" + File.separator + className + "Mapper.java"
        }
        return if ("Repository" == templateName) {
            packagePath + "repository" + File.separator + className + "Repository.java"
        } else null
    }

    /**
     * 定义前端文件路径以及名称
     */
    private fun getFrontFilePath(templateName: String, apiPath: String, path: String, apiName: String): String? {
        if ("api" == templateName) {
            return apiPath + File.separator + apiName + ".js"
        }
        return if ("index" == templateName) {
            path + File.separator + "index.vue"
        } else null
    }

    @Throws(IOException::class)
    private fun genFile(file: File, template: Template, map: Map<String?, Any?>) {
        // 生成目标文件
        var writer: Writer? = null
        try {
            FileUtils.touch(file)
            writer = FileWriter(file)
            template.render(map, writer)
        } catch (e: TemplateException) {
            throw UtilsException(e.message)
        } catch (e: IOException) {
            throw UtilsException(e.message)
        } finally {
            assert(writer != null)
            writer!!.close()
        }
    }
}
