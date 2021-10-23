package com.lwohvye.handler;

import com.alibaba.fastjson.parser.ParserConfig;
import org.springframework.security.authentication.jaas.JaasGrantedAuthority;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.web.authentication.switchuser.SwitchUserGrantedAuthority;



// https://github.com/alibaba/fastjson/wiki/fastjson_safemode
// 这部分未用到，仅作为示例使用
public class GrantedAuthorityAutoTypeCheckHandler implements ParserConfig.AutoTypeCheckHandler {

        public Class<?> handler(String typeName, Class<?> expectClass, int features) {
            return switch (typeName) {
                case "JaasGrantedAuthority" -> JaasGrantedAuthority.class;
                case "SimpleGrantedAuthority" -> SimpleGrantedAuthority.class;
                case "SwitchUserGrantedAuthority" -> SwitchUserGrantedAuthority.class;
                default -> GrantedAuthority.class;
            };
        }
    }
