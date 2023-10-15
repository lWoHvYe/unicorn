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
package com.lwohvye.sys.modules.security.core;

import cn.hutool.core.util.IdUtil;
import com.lwohvye.core.utils.DateUtils;
import com.lwohvye.sys.modules.mnt.websocket.MsgType;
import com.lwohvye.sys.modules.mnt.websocket.SocketMsg;
import com.lwohvye.sys.modules.mnt.websocket.WebSocketServer;
import com.lwohvye.sys.modules.security.config.bean.SecurityProperties;
import com.lwohvye.sys.modules.security.service.dto.JwtUserDto;
import com.lwohvye.sys.modules.security.utils.SecuritySysUtil;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RedissonClient;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import javax.crypto.SecretKey;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.concurrent.TimeUnit;

/**
 * @author /
 */
@Slf4j
@Component
@RequiredArgsConstructor
// InitializingBean的用法基本上与@PostConstruct一致，只不过相应的Bean需要实现afterPropertiesSet方法。用于在bean初始化之后执行一些操作
public class TokenProvider {

    private final SecurityProperties properties;
    private final RedissonClient redisson;
    private final UserDetailsService userDetailsService;
    public static final String AUTHORITIES_KEY = "user";

    public static final SecretKey SECRET_KEY = Jwts.SIG.HS256.key().build();

    /**
     * 创建Token ，
     * JWT是由三段信息构成的，将这三段信息文本用.链接一起就构成了Jwt字符串
     * 第一部分称为头部（header),第二部分称其为载荷（payload, 类似于飞机上承载的物品)，第三部分是签证（signature).
     * header，jwt的头部承载两部分信息：声明类型，这里是jwt、声明加密的算法（通常直接使用 HMAC SHA256）。对其进行base64编码，得到第一部分
     * payload，载荷就是存放有效信息的地方。这个名字像是特指飞机上承载的货品，这些有效信息包含三个部分:标准中注册的声明、公共的声明、私有的声明。对其进行base64编码，得到第二部分
     * - 有效载荷部分，是JWT的主体内容部分，也是一个JSON对象，包含需要传递的数据。 JWT指定七个默认字段供选择
     * --  iss: jwt签发者
     * -- sub: jwt所面向，使用jwt的用户
     * -- aud: 接收jwt的一方
     * -- exp: jwt的过期时间，这个过期时间必须大于签发时间
     * -- nbf: 定义在指定时间之前，该jwt都是不可用的.
     * -- iat: jwt的签发时间
     * -- jti: jwt的唯一身份标识，主要用来作为一次性token,从而回避重放攻击
     * -- 除以上默认字段外，还可以自定义私有字段，可以用来存一些必要但非敏感的信息
     * - 对于已签名的令牌，此信息尽管可以防止篡改，但任何人都可以读取。除非将其加密，否则请勿将重要信息放入JWT的有效负载或报头元素中（header和payload都是base64编码。盐secret是用于签名的，所以前面两部分没太大的安全性）
     * - 载荷部分存在两个属性：payload和claims。两个属性均可作为载荷，jjwt中二者只能设置其一，如果同时设置，在终端方法compact() 中将抛出异常
     * signature,jwt的第三部分是一个签证信息，这个签证信息由三部分组成：header (base64后的)、payload (base64后的)、secret（盐，不可泄漏）。base64编码后的header和base64编码后的payload使用.连接组成的字符串，然后通过header中声明的加密方式进行加盐secret组合加密，就构成了jwt的第三部分。
     * JWT的特点是无状态的，所以无法解决主动过期及续期的问题（续期实际上是重新颁发token）
     * 更多💻可参考：https://www.lwohvye.com/2021/11/12/jjwt%e7%9b%b8%e5%85%b3%e7%ac%94%e8%ae%b0/
     *
     * @param authentication /
     * @return /
     */
    public String createToken(Authentication authentication) {
        var curDate = LocalDateTime.now();
        final Date expirationDate = DateUtils.toDate(curDate.plusSeconds(properties.getTokenValidityInSeconds()));
        return Jwts.builder()
                // 加入ID确保生成的 Token 都不一致
                .id(IdUtil.simpleUUID())
                // 签发者
                .issuer("lWoHvYe")
                // 私有声明。权限作为偏动态的，不放入token中
                .claim(AUTHORITIES_KEY, authentication.getName())
                // 这里放入了username。然后在 getAuthentication()中，解密并取出来，构建了Authentication。
                // 在doFilter()中，将Authentication存入上下文。SecurityContextHolder.getContext().setAuthentication(authentication);
                // 在getCurrentUser()中，从上下文中取出Authentication，然后根据其中的username，通过方法获取用户信息并返回。userDetailsService.loadUserByUsername(getCurrentUsername());
                // 所以请求携带的token中，比较主要的属性就是username。用户的具体信息，都是通过用户名称去方法中获取的。这样做使得在用户的角色权限等变更时，原token可继续使用，且权限已为最新的
                .subject(authentication.getName())
                // 设置颁发时间
                .issuedAt(DateUtils.toDate(curDate))
                // 设置过期时间，
                .expiration(expirationDate)
                .signWith(SECRET_KEY)
                .compact();
    }

    /**
     * 依据Token 获取鉴权信息
     *
     * @param token /
     * @return /
     */
    public Authentication getAuthentication(String token) {
        // 上面createToken()中jwtBuilder中设置的属性，都在token中，解密后，得到Claims。这里用到了其subject属性，在当前业务里存的用户名
        Claims claims = getClaims(token);
        //  第三个参数是 <? extends GrantedAuthority> authorities ,即为用户的权限。当前改为角色级别
        var authorities = userDetailsService.loadUserByUsername(claims.getSubject()).getAuthorities();
        User principal = new User(claims.getSubject(), "******", authorities);
        //  同上，这里第三个参数也是用户的权限。
        return new UsernamePasswordAuthenticationToken(principal, token, authorities);
    }

    public Claims getClaims(String token) {
        // 解密的算法由header中指定，后续看看有没有办法固定化。加密是🧷的
        return Jwts.parser()
                // .keyLocator(keyLocator) // (2) dynamically locate signing or encryption keys
                .verifyWith(SECRET_KEY)      //     or a constant key used to verify all signed JWTs
                //.decryptWith(key)     //     or a constant key used to decrypt all encrypted JWTs
                .build()
                .parseSignedClaims(token)
                .getPayload();
    }

    /**
     * 初步检测并获取Token
     *
     * @param request /
     * @return /
     */
    public String getToken(HttpServletRequest request) {
        String bearerToken = request.getHeader(properties.getHeader());
        if (StringUtils.hasText(bearerToken) && bearerToken.startsWith(properties.getTokenStartWith())) {
            // 去掉令牌前缀
            return bearerToken.replace(properties.getTokenStartWith(), "");
        } else {
            log.debug("非法Token：{}", bearerToken);
        }
        return null;
    }

    // region 校验

    public Boolean validateToken(String token, JwtUserDto jwtUserDto) {
        var claims = getClaims(token);
        // 颁发时间
        var issuedAt = claims.getIssuedAt();
        // 过期时间。JWT在认证时，会在内部校验和处理过期问题
//        var expiration = claims.getExpiration();
//        如果token创建日期 > 最后修改密码的日期 则代表token有效
        return !isCreatedBeforeLastPasswordReset(issuedAt, jwtUserDto.getUser().getPwdResetTime());
    }

    private Boolean isCreatedBeforeLastPasswordReset(Date issuedAt, Date lastPasswordReset) {
        return (lastPasswordReset != null && issuedAt.before(lastPasswordReset));
    }

    // endregion

    // region ⏰即将过期
    // 先validate通过。若即将过期，进行一次通知
    public void noticeExpire5Token(String token) {
        var curDate = LocalDateTime.now();
        var claims = getClaims(token);
        var expirationDate = DateUtils.toLocalDateTime(claims.getExpiration());
        if (curDate.plusSeconds(properties.getDetect()).isAfter(expirationDate)) {
            // 已通知过，跳过
            var rMapCache = redisson.getMapCache(SecuritySysUtil.getExpireNoticeKey(properties));
            // RMapCache，可以对单key设置过期时间
            // 使用fastPutIfAbsent。当key不存在时，设置值。成功设置时返回true
            var putResult = rMapCache.fastPutIfAbsent(token, LocalDateTime.now().toString(), properties.getDetect(), TimeUnit.SECONDS);
            if (Boolean.TRUE.equals(putResult)) {
                try {
                    // 提醒
                    WebSocketServer.sendInfo(new SocketMsg("您的余额已不足，请及时充值", MsgType.INFO), "sysMember");
                } catch (IOException e) {
                    log.error("系统通知失败：{} ", e.getMessage());
                }
            }
        }
    }
    // endregion
}
