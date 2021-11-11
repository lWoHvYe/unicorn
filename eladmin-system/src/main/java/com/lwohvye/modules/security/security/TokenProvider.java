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
import com.lwohvye.modules.security.service.dto.JwtUserDto;
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
import java.util.Date;
import java.util.concurrent.TimeUnit;

/**
 * @author /
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class TokenProvider implements InitializingBean {

    private final SecurityProperties properties;
    private final RedisUtils redisUtils;
    public static final String AUTHORITIES_KEY = "user";
    private final Clock clock = DefaultClock.INSTANCE;
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
                .signWith(key, SignatureAlgorithm.HS512);
    }

    /**
     * 创建Token 设置永不过期，
     * Token 的时间有效性转到Redis 维护
     * JWT是由三段信息构成的，将这三段信息文本用.链接一起就构成了Jwt字符串
     * 第一部分称为头部（header),第二部分称其为载荷（payload, 类似于飞机上承载的物品)，第三部分是签证（signature).
     * header，jwt的头部承载两部分信息：声明类型，这里是jwt、声明加密的算法（通常直接使用 HMAC SHA256）
     * payload，载荷就是存放有效信息的地方。这个名字像是特指飞机上承载的货品，这些有效信息包含三个部分:标准中注册的声明、公共的声明、私有的声明
     * signature,jwt的第三部分是一个签证信息，这个签证信息由三部分组成：header (base64后的)、payload (base64后的)、secret（盐，不可泄漏）
     * JWT的特点是无状态的，所以无法解决主动过期及续期的问题（续期实际上是重新颁发token）
     *
     * @param authentication /
     * @return /
     */
    public String createToken(Authentication authentication) {
        var curDate = clock.now();
        final Date expirationDate = calculateExpirationDate(curDate);
        return jwtBuilder
                // 加入ID确保生成的 Token 都不一致
                .setId(IdUtil.simpleUUID())
                .claim(AUTHORITIES_KEY, authentication.getName())
                // 这里放入了username。然后在 getAuthentication()中，解密并取出来，构建了Authentication。
                // 在doFilter()中，将Authentication存入上下文。SecurityContextHolder.getContext().setAuthentication(authentication);
                // 在getCurrentUser()中，从上下文中取出Authentication，然后根据其中的username，通过方法获取用户信息并返回。userDetailsService.loadUserByUsername(getCurrentUsername());
                // 所以请求携带的token中，比较主要的属性就是username。用户的具体信息，都是通过用户名称去方法中获取的。
                // 另外以token为key存入redis的值的具体内容，当前只在查看在线用户时用到。在鉴权等时，只判断key是否存在，因为key是很难被伪造的，所以默认key中的信息就是正确的。
                // 当前，也可以比较下key上的和取的值是不是同一个user，这个可后续再调整。
                .setSubject(authentication.getName())
                // 设置颁发时间
                .setIssuedAt(curDate)
                // 设置过期时间，
                .setExpiration(expirationDate)
                .compact();
    }

    private Date calculateExpirationDate(Date createdDate) {
        return new Date(createdDate.getTime() + properties.getRenew());
    }

    /**
     * 依据Token 获取鉴权信息
     *
     * @param token /
     * @return /
     */
    Authentication getAuthentication(String token) {
        // 上面createToken()中jwtBuilder中设置的属性，都在token中，解密后，得到Claims。这里用到了其subject属性，在当前业务里存的用户名
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

    // region 校验

    public Boolean validateToken(String token, JwtUserDto jwtUserDto) {
        var claims = getClaims(token);
        // 颁发时间
        var issuedAt = claims.getIssuedAt();
        // 过期时间
        var expiration = claims.getExpiration();
//        如果token未过期，且token创建日期 > 最后修改密码的日期 则代表token有效
        return !(isTokenExpired(expiration) || isCreatedBeforeLastPasswordReset(issuedAt, jwtUserDto.getUser().getPwdResetTime()));
    }

    // token是否过期
    private Boolean isTokenExpired(Date expiration) {
        // before在之前
        return expiration.before(clock.now());
    }

    private Boolean isCreatedBeforeLastPasswordReset(Date issuedAt, Date lastPasswordReset) {
        return (lastPasswordReset != null && issuedAt.before(lastPasswordReset));
    }

    // endregion

    // region 续期
    // 先validate通过，再校验续期。若续期了，要重新设置header
    public String refreshToken(String token) {
        var curDate = clock.now();
        var expirationDate = calculateExpirationDate(curDate);

        var claims = getClaims(token);
        var expiration = claims.getExpiration();
        if (expiration.getTime() - curDate.getTime() < properties.getDetect()) {
            // 更新过期时间
            claims.setExpiration(expirationDate);

            return jwtBuilder
                    .setClaims(claims)
                    .compact();
        }
        return null;
    }

    // endregion
}
