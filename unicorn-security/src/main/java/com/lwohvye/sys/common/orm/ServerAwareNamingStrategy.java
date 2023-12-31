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
package com.lwohvye.sys.common.orm;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.boot.model.naming.CamelCaseToUnderscoresNamingStrategy;
import org.hibernate.boot.model.naming.Identifier;
import org.hibernate.engine.jdbc.env.spi.JdbcEnvironment;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.expression.BeanFactoryAccessor;
import org.springframework.context.expression.BeanFactoryResolver;
import org.springframework.core.env.Environment;
import org.springframework.expression.ParserContext;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.stereotype.Component;

/**
 * Jpa在映射实体和表的关系时，提供了一个入口，用于对绑定做一些操作。SpringPhysicalNamingStrategy标记了过期，
 * 可考虑继承hibernate侧的相关实现 {@link org.hibernate.boot.model.naming.CamelCaseToUnderscoresNamingStrategy}，未验证
 *
 * @author Hongyan Wang
 */
@Slf4j
@Component
public class ServerAwareNamingStrategy extends CamelCaseToUnderscoresNamingStrategy implements ApplicationContextAware {

    @Autowired
    private Environment env;

    private final StandardEvaluationContext context = new StandardEvaluationContext();

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.context.addPropertyAccessor(new BeanFactoryAccessor());
        this.context.setBeanResolver(new BeanFactoryResolver(applicationContext));
        this.context.setRootObject(applicationContext);
    }

    /**
     * 使用映射表名后，需注意。尽量不要用原生sql,要用hql替代
     * 除了@Table注解。@JoinTable注解也试用
     *
     * @param name
     * @param jdbcEnvironment
     * @return org.hibernate.boot.model.naming.Identifier
     * @date 2021/4/23 11:06 上午
     */
    @Override
    public Identifier toPhysicalTableName(Identifier name, JdbcEnvironment jdbcEnvironment) {
        String nameStr = name.getText();
//        格式 #{xxx:y}  获取xxx的值，若获取不到使用y的值
        var prefix = ParserContext.TEMPLATE_EXPRESSION.getExpressionPrefix();
        var suffix = ParserContext.TEMPLATE_EXPRESSION.getExpressionSuffix();
        if (StrUtil.isNotBlank(nameStr) && nameStr.startsWith(prefix) && nameStr.endsWith(suffix)) {
            var index = nameStr.indexOf(":");
            var key = nameStr.substring(2, ObjectUtil.equal(index, -1) ? nameStr.length() - 1 : index);
            var defaultValue = ObjectUtil.equal(index, -1) ? "" : nameStr.substring(index + 1, nameStr.length() - 1);
            var value = env.getProperty(key);
            //参考SimpleElasticsearchPersistentEntity 实现思想,将tableName参数的值支持表达式获取
//            Expression expression = this.parser.parseExpression(prefix + key + suffix, ParserContext.TEMPLATE_EXPRESSION);
//            log.error(expression.getValue(this.context, String.class));
            return Identifier.toIdentifier(StrUtil.isNotBlank(value) ? value : defaultValue);
        } else {
            //默认方式不变
            return super.toPhysicalTableName(name, jdbcEnvironment);
        }
    }
}
