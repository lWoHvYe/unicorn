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
package com.lwohvye.modules.security.security;

import cn.hutool.core.util.IdUtil;
import com.lwohvye.modules.security.config.bean.SecurityProperties;
import com.lwohvye.modules.security.utils.SecuritySysUtil;
import com.lwohvye.utils.redis.RedisUtils;
import io.jsonwebtoken.*;
import io.jsonwebtoken.impl.DefaultClock;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.User;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;
import java.security.Key;
import java.util.ArrayList;
import java.util.concurrent.TimeUnit;

/**
 * @author /
 */
@Slf4j
@Component
@RequiredArgsConstructor
// InitializingBean的用法基本上与@PostConstruct一致，只不过相应的Bean需要实现afterPropertiesSet方法。用于在bean初始化之后执行一些操作
public class TokenProvider implements InitializingBean {

    private final SecurityProperties properties;
    private final RedisUtils redisUtils;
    public static final String AUTHORITIES_KEY = "user";
    private static final Clock clock = DefaultClock.INSTANCE;
    private JwtParser jwtParser;
    private JwtBuilder jwtBuilder;

    @Override
    public void afterPropertiesSet() {
        byte[] keyBytes = Decoders.BASE64.decode(properties.getBase64Secret());
        Key key = Keys.hmacShaKeyFor(keyBytes);
        jwtParser = Jwts.parserBuilder()
                .setSigningKey(key)
                .build();
        jwtBuilder = Jwts.builder()
                // 这里指定了加密算法和密钥
                .signWith(key, SignatureAlgorithm.HS512);
    }

    /**
     * 创建Token 设置永不过期，
     * Token 的时间有效性转到Redis 维护
     * JWT是由三段信息构成的，将这三段信息文本用.链接一起就构成了Jwt字符串
     * 第一部分称为头部（header),第二部分称其为载荷（payload, 类似于飞机上承载的物品)，第三部分是签证（signature).
     * header，jwt的头部承载两部分信息：声明类型，这里是jwt、声明加密的算法（通常直接使用 HMAC SHA256）。对其进行base64加密（可以是对称加密），得到第一部分
     * payload，载荷就是存放有效信息的地方。这个名字像是特指飞机上承载的货品，这些有效信息包含三个部分:标准中注册的声明、公共的声明、私有的声明。对其进行base64加密，得到第二部分
     *  有效载荷部分，是JWT的主体内容部分，也是一个JSON对象，包含需要传递的数据。 JWT指定七个默认字段供选择
     *      iss: jwt签发者
     *      sub: jwt所面向，使用jwt的用户
     *      aud: 接收jwt的一方
     *      exp: jwt的过期时间，这个过期时间必须大于签发时间
     *      nbf: 定义在指定时间之前，该jwt都是不可用的.
     *      iat: jwt的签发时间
     *      jti: jwt的唯一身份标识，主要用来作为一次性token,从而回避重放攻击
     *      除以上默认字段外，还可以自定义私有字段，可以用来存一些必要但非敏感的信息
     *   对于已签名的令牌，此信息尽管可以防止篡改，但任何人都可以读取。除非将其加密，否则请勿将重要信息放入JWT的有效负载或报头元素中（header和payload都是base64编码。盐secret是用于签名的，所以前面两部分没太大的安全性）
     *  载荷部分存在两个属性：payload和claims。两个属性均可作为载荷，jjwt中二者只能设置其一，如果同时设置，在终端方法compact() 中将抛出异常
     * signature,jwt的第三部分是一个签证信息，这个签证信息由三部分组成：header (base64后的)、payload (base64后的)、secret（盐，不可泄漏）。base64加密后的header和base64加密后的payload使用.连接组成的字符串，然后通过header中声明的加密方式进行加盐secret组合加密，就构成了jwt的第三部分。
     * JWT的特点是无状态的，所以无法解决主动过期及续期的问题（续期实际上是重新颁发token）
     * 所以，当前JWT只是拿来当个key，主体信息还在服务侧存储，从用法上看姿势有点不对,这里需注意
     * 更多💻可参考：https://www.lwohvye.com/2021/11/12/jjwt%e7%9b%b8%e5%85%b3%e7%ac%94%e8%ae%b0/
     * @param authentication /
     * @return /
     */
    public String createToken(Authentication authentication) {
        var curDate = clock.now();
        return jwtBuilder
                // 加入ID确保生成的 Token 都不一致
                .setId(IdUtil.simpleUUID())
                .claim(AUTHORITIES_KEY, authentication.getName())
                // 这里放入了username。然后在 getAuthentication()中，解密并取出来，构建了Authentication。
                // 在doFilter()中，将Authentication存入上下文。SecurityContextHolder.getContext().setAuthentication(authentication);
                // 在getCurrentUser()中，从上下文中取出Authentication，然后根据其中的username，通过方法获取用户信息并返回。userDetailsService.loadUserByUsername(getCurrentUsername());
                // 所以请求携带的token中，比较主要的属性就是username。用户的具体信息，都是通过用户名称去方法中获取的。这样做使得在用户的角色权限等变更时，原token可继续使用，且权限已为最新的
                // 另外以token为key存入redis的值的具体内容，当前只在查看在线用户时用到。在鉴权等时，只判断key是否存在，因为key是很难被伪造的，所以默认key中的信息就是正确的。
                .setSubject(authentication.getName())
                // 设置颁发时间
                .setIssuedAt(curDate)
                // 设置过期时间，
//                .setExpiration(expirationDate)
                .compact();
    }

    /**
     * 依据Token 获取鉴权信息
     *
     * @param token /
     * @return /
     */
    Authentication getAuthentication(String token) {
        Claims claims = getClaims(token);
        User principal = new User(claims.getSubject(), "******", new ArrayList<>());
        return new UsernamePasswordAuthenticationToken(principal, token, new ArrayList<>());
    }

    public Claims getClaims(String token) {
        return jwtParser
                .parseClaimsJws(token)
                .getBody();
    }

    /**
     * @param token 需要检查的token
     */
    public void checkRenewal(String token) {
        // 判断是否续期token,计算token的过期时间
        long expireTime = redisUtils.getExpire(SecuritySysUtil.getAuthToken(properties, token)) * 1000;
//        Date expireDate = DateUtil.offset(new Date(), DateField.MILLISECOND, (int) expireTime);
        // 判断当前时间与过期时间的时间差
//        long differ = expireDate.getTime() - System.currentTimeMillis();
        // 如果在续期检查的范围内，则续期
        // 2021/6/30 time和differ理论上是一样的。可略去部分逻辑
//        if (differ <= properties.getDetect()) {
        if (expireTime <= properties.getDetect()) {
            long renew = expireTime + properties.getRenew();
            redisUtils.expire(SecuritySysUtil.getAuthToken(properties, token), renew, TimeUnit.MILLISECONDS);
        }
    }

    public String getToken(HttpServletRequest request) {
        final String requestHeader = request.getHeader(properties.getHeader());
        if (requestHeader != null && requestHeader.startsWith(properties.getTokenStartWith())) {
            return requestHeader.substring(7);
        }
        return null;
    }
}
