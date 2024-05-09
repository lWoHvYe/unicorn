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
package com.lwohvye.core.utils;

import cn.hutool.core.util.StrUtil;
import cn.hutool.http.HttpUtil;
import cn.hutool.http.useragent.UserAgentUtil;
import cn.hutool.json.JSONException;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import com.lwohvye.core.constant.UnicornCoreConstant;
import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import net.dreamlu.mica.ip2region.core.Ip2regionSearcher;
import net.dreamlu.mica.ip2region.core.IpInfo;
import org.springframework.lang.NonNull;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.util.Calendar;
import java.util.Date;
import java.util.Enumeration;

/**
 * @author Zheng Jie
 * 字符串工具类, 继承org.apache.commons.lang3.StringUtils类
 */
@Slf4j
public class StringUtils extends org.apache.commons.lang3.StringUtils {

    private static final String UNKNOWN = "unknown";

    /**
     * 注入bean
     */
    private static final Ip2regionSearcher IP_SEARCHER = SpringContextHolder.getBean(Ip2regionSearcher.class);

    /**
     * 获取ip地址
     */
    public static String getIp(HttpServletRequest request) {
        String ip = request.getHeader("x-forwarded-for");
        if (ip == null || ip.length() == 0 || UNKNOWN.equalsIgnoreCase(ip)) {
            ip = request.getHeader("Proxy-Client-IP");
        }
        if (ip == null || ip.length() == 0 || UNKNOWN.equalsIgnoreCase(ip)) {
            ip = request.getHeader("WL-Proxy-Client-IP");
        }
        if (ip == null || ip.length() == 0 || UNKNOWN.equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }
        String comma = ",";
        String localhost = "127.0.0.1";
        if (ip.contains(comma)) {
            ip = ip.split(",")[0];
        }
        if (localhost.equals(ip)) {
            // 获取本机真正的ip地址
            try {
                ip = InetAddress.getLocalHost().getHostAddress();
            } catch (UnknownHostException e) {
                log.error(e.getMessage(), e);
            }
        }
        return ip;
    }

    /**
     * 根据ip获取详细地址
     */
    public static String getHttpCityInfo(String ip) {
        String api = String.format(UnicornCoreConstant.Url.IP_URL, ip);
        try {
            JSONObject object = JSONUtil.parseObj(HttpUtil.get(api));
            return object.get("addr", String.class);
        } catch (JSONException e) {
            return "Moon";
        }
    }

    /**
     * 根据ip获取详细地址
     */
    public static String getLocalCityInfo(String ip) {
        IpInfo ipInfo = IP_SEARCHER.memorySearch(ip);
        if (ipInfo != null) {
            return ipInfo.getAddress();
        }
        return null;

    }

    public static String getBrowser(HttpServletRequest request) {
        var ua = UserAgentUtil.parse(request.getHeader("User-Agent"));
        return String.format("%s: %s-%s", ua.getPlatform(), ua.getBrowser(), ua.getVersion());
    }

    /**
     * 获得当天是周几
     */
    public static String getWeekDay() {
        String[] weekDays = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
        Calendar cal = Calendar.getInstance();
        cal.setTime(new Date());

        int w = cal.get(Calendar.DAY_OF_WEEK) - 1;
        if (w < 0) {
            w = 0;
        }
        return weekDays[w];
    }

    /**
     * 获取当前机器的IP
     *
     * @return /
     */
    public static String getLocalIp() {
        try {
            InetAddress candidateAddress = null;
            // 遍历所有的网络接口
            for (Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces(); interfaces.hasMoreElements(); ) {
                NetworkInterface anInterface = interfaces.nextElement();
                // 在所有的接口下再遍历IP
                for (Enumeration<InetAddress> inetAddresses = anInterface.getInetAddresses(); inetAddresses.hasMoreElements(); ) {
                    InetAddress inetAddr = inetAddresses.nextElement();
                    // 排除loopback类型地址
                    if (!inetAddr.isLoopbackAddress()) {
                        if (inetAddr.isSiteLocalAddress()) {
                            // 如果是site-local地址，就是它了
                            return inetAddr.getHostAddress();
                        } else if (candidateAddress == null) {
                            // site-local类型的地址未被发现，先记录候选地址
                            candidateAddress = inetAddr;
                        }
                    }
                }
            }
            if (candidateAddress != null) {
                return candidateAddress.getHostAddress();
            }
            // 如果没有发现 non-loopback地址.只能用最次选的方案
            InetAddress jdkSuppliedAddress = InetAddress.getLocalHost();
            if (jdkSuppliedAddress == null) {
                return "";
            }
            return jdkSuppliedAddress.getHostAddress();
        } catch (Exception e) {
            return "";
        }
    }

    /**
     * 将使用ISO-8859-1编码导致乱码的中文，使用UTF-8重新编码
     * 当前用于使用String存储数据库中blob类型属性时，解决返回数据乱码问题
     *
     * @return java.lang.String
     * @params [str]
     * @date 2020/11/13 9:40
     */
    public static String convertToString(String str) {
        return StrUtil.isNotEmpty(str) ? new String(str.getBytes(StandardCharsets.ISO_8859_1)) : "";
    }

    /**
     * 校验是否是合法的ip地址。针对复杂字符串的校验问题，要想到正则
     *
     * @param str
     * @return boolean
     * @date 2022/3/16 4:46 PM
     */
    public static boolean isIPAddress(@NonNull String str) {
        String regx = "((2(5[0-5]|[0-4]\\d))|[0-1]?\\d{1,2})(\\.((2(5[0-5]|[0-4]\\d))|[0-1]?\\d{1,2})){3}";
        return str.matches(regx);
    }
}
