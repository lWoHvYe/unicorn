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

import org.apache.commons.codec.binary.Base64;

import javax.crypto.Cipher;
import javax.crypto.spec.OAEPParameterSpec;
import javax.crypto.spec.PSource;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.security.*;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.MGF1ParameterSpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

/**
 * Rsa 工具类，公钥私钥生成，加解密
 *
 * @author https://www.cnblogs.com/nihaorz/p/10690643.html
 * @date 2020-05-18
 **/
public class RsaUtils {

    private static final String SRC = "123456";

    // 签名算法
    private static final String CIPHER_ALGORITHM = "RSA/ECB/PKCS1Padding";

    public static void main(String[] args) throws Exception {
        System.out.println("\n");
        RsaKeyPair keyPair = generateKeyPair();
        System.out.println("公钥：" + keyPair.getPublicKey());
        System.out.println("私钥：" + keyPair.getPrivateKey());
        System.out.println("\n");
        test1(keyPair);
        System.out.println("\n");
        test2(keyPair);
        System.out.println("\n");
    }

    /**
     * 公钥加密私钥解密
     */
    private static void test1(RsaKeyPair keyPair) throws Exception {
        System.out.println("***************** 公钥加密私钥解密开始 *****************");
        String text1 = encryptByPublicKey(keyPair.getPublicKey(), RsaUtils.SRC);
        String text2 = decryptByPrivateKey(keyPair.getPrivateKey(), text1);
        System.out.println("加密前：" + RsaUtils.SRC);
        System.out.println("加密后：" + text1);
        System.out.println("解密后：" + text2);
        if (RsaUtils.SRC.equals(text2)) {
            System.out.println("解密字符串和原始字符串一致，解密成功");
        } else {
            System.out.println("解密字符串和原始字符串不一致，解密失败");
        }
        System.out.println("***************** 公钥加密私钥解密结束 *****************");
    }

    /**
     * 私钥加密公钥解密
     *
     * @throws Exception /
     */
    private static void test2(RsaKeyPair keyPair) throws Exception {
        System.out.println("***************** 私钥加密公钥解密开始 *****************");
        String text1 = encryptByPrivateKey(keyPair.getPrivateKey(), RsaUtils.SRC);
        String text2 = decryptByPublicKey(keyPair.getPublicKey(), text1);
        System.out.println("加密前：" + RsaUtils.SRC);
        System.out.println("加密后：" + text1);
        System.out.println("解密后：" + text2);
        if (RsaUtils.SRC.equals(text2)) {
            System.out.println("解密字符串和原始字符串一致，解密成功");
        } else {
            System.out.println("解密字符串和原始字符串不一致，解密失败");
        }
        System.out.println("***************** 私钥加密公钥解密结束 *****************");
    }

    /**
     * 公钥解密
     *
     * @param publicKeyText   公钥
     * @param sourceBase64RSA 待解密的信息
     * @return /
     * @throws Exception /
     */
    public static String decryptByPublicKey(String publicKeyText, String sourceBase64RSA) throws Exception {
        var x509EncodedKeySpec = new X509EncodedKeySpec(Base64.decodeBase64(publicKeyText));
        var keyFactory = KeyFactory.getInstance("RSA");
        var publicKey = keyFactory.generatePublic(x509EncodedKeySpec);
        var cipher = Cipher.getInstance(CIPHER_ALGORITHM);
        cipher.init(Cipher.DECRYPT_MODE, publicKey);
        var result = cipher.doFinal(Base64.decodeBase64(sourceBase64RSA));
        return new String(result, StandardCharsets.UTF_8);
    }

    /**
     * 私钥加密
     *
     * @param privateKeyText 私钥
     * @param data           待加密的信息
     * @return /
     * @throws Exception /
     */
    public static String encryptByPrivateKey(String privateKeyText, String data) throws Exception {
        var pkcs8EncodedKeySpec = new PKCS8EncodedKeySpec(Base64.decodeBase64(privateKeyText));
        var keyFactory = KeyFactory.getInstance("RSA");
        var privateKey = keyFactory.generatePrivate(pkcs8EncodedKeySpec);
        var cipher = Cipher.getInstance(CIPHER_ALGORITHM);
        cipher.init(Cipher.ENCRYPT_MODE, privateKey);
        var result = cipher.doFinal(data.getBytes(StandardCharsets.UTF_8));
        return Base64.encodeBase64String(result);
    }

    /**
     * 私钥解密
     *
     * @param privateKeyText  私钥
     * @param sourceBase64RSA 待解密的文本
     * @return /
     * @throws Exception /
     */
    public static String decryptByPrivateKey(String privateKeyText, String sourceBase64RSA) throws Exception {
        var pkcs8EncodedKeySpec = new PKCS8EncodedKeySpec(Base64.decodeBase64(privateKeyText));
        var keyFactory = KeyFactory.getInstance("RSA");
        var privateKey = keyFactory.generatePrivate(pkcs8EncodedKeySpec);
        var cipher = Cipher.getInstance(CIPHER_ALGORITHM);
        cipher.init(Cipher.DECRYPT_MODE, privateKey);
        var result = cipher.doFinal(Base64.decodeBase64(sourceBase64RSA));
        return new String(result, StandardCharsets.UTF_8);
    }

    /**
     * 公钥加密
     *
     * @param publicKeyText 公钥
     * @param data          待加密的文本
     * @return /
     */
    public static String encryptByPublicKey(String publicKeyText, String data) throws Exception {
        var x509EncodedKeySpec = new X509EncodedKeySpec(Base64.decodeBase64(publicKeyText));
        var keyFactory = KeyFactory.getInstance("RSA");
        var publicKey = keyFactory.generatePublic(x509EncodedKeySpec);
        var cipher = Cipher.getInstance(CIPHER_ALGORITHM);
        cipher.init(Cipher.ENCRYPT_MODE, publicKey);
        var result = cipher.doFinal(data.getBytes(StandardCharsets.UTF_8));
        return Base64.encodeBase64String(result);
    }

    private static byte[] doLongerCipherFinal(int opMode, Cipher cipher, byte[] source) throws Exception {
        try (var out = new ByteArrayOutputStream()) {
            if (opMode == Cipher.DECRYPT_MODE) {
                out.write(cipher.doFinal(source));
            } else {
                int offset = 0;
                int totalSize = source.length;
                while (totalSize - offset > 0) {
                    int size = Math.min(cipher.getOutputSize(0) - 11, totalSize - offset);
                    out.write(cipher.doFinal(source, offset, size));
                    offset += size;
                }
            }
            return out.toByteArray();
        }
    }

    /**
     * 构建RSA密钥对
     *
     * @return /
     * @throws NoSuchAlgorithmException /
     */
    public static RsaKeyPair generateKeyPair() throws NoSuchAlgorithmException {
        var keyPairGenerator = KeyPairGenerator.getInstance("RSA");
        keyPairGenerator.initialize(2048);
        var keyPair = keyPairGenerator.generateKeyPair();
        var rsaPublicKey = (RSAPublicKey) keyPair.getPublic();
        var rsaPrivateKey = (RSAPrivateKey) keyPair.getPrivate();
        var publicKeyString = Base64.encodeBase64String(rsaPublicKey.getEncoded());
        var privateKeyString = Base64.encodeBase64String(rsaPrivateKey.getEncoded());
        return new RsaKeyPair(publicKeyString, privateKeyString);
    }

    // region OAEP
    // java.security.InvalidKeyException: OAEP cannot be used to sign or verify signatures, so it just support encrypt by publicKey and decrypt by privateKey
    public static String encrypt(String publicKeyText, String data) throws Exception {
        var x509EncodedKeySpec = new X509EncodedKeySpec(Base64.decodeBase64(publicKeyText));
        var keyFactory = KeyFactory.getInstance("RSA");
        var publicKey = keyFactory.generatePublic(x509EncodedKeySpec);
        var cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA-1AndMGF1Padding");
        var oaepParams = new OAEPParameterSpec("SHA1", "MGF1", new MGF1ParameterSpec("SHA-1"), PSource.PSpecified.DEFAULT);
        cipher.init(Cipher.ENCRYPT_MODE, publicKey, oaepParams);
        var result = cipher.doFinal(data.getBytes(StandardCharsets.UTF_8));
        return Base64.encodeBase64String(result);
    }

    public static String decrypt(String privateKeyText, String sourceBase64RSA) throws Exception {
        var pkcs8EncodedKeySpec = new PKCS8EncodedKeySpec(Base64.decodeBase64(privateKeyText));
        var keyFactory = KeyFactory.getInstance("RSA");
        var privateKey = keyFactory.generatePrivate(pkcs8EncodedKeySpec);
        var cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA-1AndMGF1Padding");
        var oaepParams = new OAEPParameterSpec("SHA1", "MGF1", new MGF1ParameterSpec("SHA-1"), PSource.PSpecified.DEFAULT);
        cipher.init(Cipher.DECRYPT_MODE, privateKey, oaepParams);
        var result = cipher.doFinal(Base64.decodeBase64(sourceBase64RSA));
        return new String(result, StandardCharsets.UTF_8);
    }
    // endregion

    /**
     * RSA密钥对对象
     */
    public static class RsaKeyPair {

        private final String publicKey;
        private final String privateKey;

        public RsaKeyPair(String publicKey, String privateKey) {
            this.publicKey = publicKey;
            this.privateKey = privateKey;
        }

        public String getPublicKey() {
            return publicKey;
        }

        public String getPrivateKey() {
            return privateKey;
        }

    }
}
