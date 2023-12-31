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
package com.lwohvye.core.utils;

import lombok.SneakyThrows;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.util.Base64;

/**
 * AES加密
 *
 * @author lWoHvYe
 * @date 2021-11-05
 */

public class EncryptUtils {

    // 只有静态变量及方法的工具类不允许进行实例化
    private EncryptUtils() {
        throw new IllegalStateException("Utility class");
    }

    private static final String STR_PARAM = "P244w0rd-lWoHvYe";

    private static Cipher cipher;

    private static final IvParameterSpec IV = new IvParameterSpec(STR_PARAM.getBytes(StandardCharsets.UTF_8));

    @SneakyThrows
    private static SecretKey getCipherAndSecretKey(String source) {
        if (source == null || source.length() == 0) {
            return null;
        }
        // 1.构造密钥生成器，指定为AES算法,不区分大小写
        var keygen = KeyGenerator.getInstance("AES");
        // 2.根据ecnodeRules规则初始化密钥生成器
        // 生成一个128位的随机源,根据传入的字节数组
        //keygen.init(128, new SecureRandom(encodeRules.getBytes()));
        var secureRandom = SecureRandom.getInstance("SHA1PRNG");
        secureRandom.setSeed(STR_PARAM.getBytes(StandardCharsets.UTF_8));
        keygen.init(128, secureRandom);
        // 3.产生原始对称密钥
        var originalKey = keygen.generateKey();
        // 4.获得原始对称密钥的字节数组
        var raw = originalKey.getEncoded();
        // 5.根据字节数组生成AES密钥
        var keySpec = new SecretKeySpec(raw, "AES");
        // 6.根据指定算法AES自成密码器
        cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        return keySpec;
    }

    /**
     * 对称加密
     */
    @SneakyThrows
    public static String aesEncrypt(String source) {
        var keySpec = getCipherAndSecretKey(source);
        // 7.初始化密码器，第一个参数为加密(Encrypt_mode)或者解密解密(Decrypt_mode)操作，第二个参数为使用的KEY
        cipher.init(Cipher.ENCRYPT_MODE, keySpec, IV);
        // 8.获取加密内容的字节数组(这里要设置为utf-8)不然内容中如果有中文和英文混合中文就会解密为乱码
        byte[] byteEncode = source.getBytes(StandardCharsets.UTF_8);
        // 9.根据密码器的初始化方式--加密：将数据加密
        byte[] byteAES = cipher.doFinal(byteEncode);
        // 10.将加密后的数据转换为字符串
        // 这里用Base64Encoder可能会找不到包
        // 解决办法：
        // 在项目的Build path中先移除JRE System Library，再添加库JRE System Library，重新编译后就一切正常了。
        return Base64.getEncoder().encodeToString(byteAES);
    }

    /**
     * 对称解密
     */
    @SneakyThrows
    public static String aesDecrypt(String source) {
        var keySpec = getCipherAndSecretKey(source);
        // 7.初始化密码器，第一个参数为加密(Encrypt_mode)或者解密(Decrypt_mode)操作，第二个参数为使用的KEY
        cipher.init(Cipher.DECRYPT_MODE, keySpec, IV);
        // 8.将加密并编码后的内容解码成字节数组
        byte[] byteContent = Base64.getDecoder().decode(source);
        // 9.解密
        byte[] byteDecode = cipher.doFinal(byteContent);
        // 10.将解密后的数据转换为字符串
        return new String(byteDecode, StandardCharsets.UTF_8);
    }
}
