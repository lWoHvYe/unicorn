package com.lwohvye.config;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.boot.model.naming.Identifier;
import org.hibernate.engine.jdbc.env.spi.JdbcEnvironment;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.orm.jpa.hibernate.SpringPhysicalNamingStrategy;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.expression.BeanFactoryAccessor;
import org.springframework.context.expression.BeanFactoryResolver;
import org.springframework.core.env.Environment;
import org.springframework.expression.ParserContext;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class ServerAwareNamingStrategy extends SpringPhysicalNamingStrategy implements ApplicationContextAware {

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
     * @description 使用映射表名后，需注意。尽量不要用原生sql,要用hql替代
     * 除了@Table注解。@JoinTable注解也试用
     * @author Hongyan Wang
     * @date 2021/4/23 11:06 上午
     * @param name
     * @param jdbcEnvironment
     * @return org.hibernate.boot.model.naming.Identifier
     */
    @Override
    public Identifier toPhysicalTableName(Identifier name, JdbcEnvironment jdbcEnvironment) {
        String nameStr = name.getText();
//        格式 #{xxx:x}  获取xxx的值，若获取不到使用x的值
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
