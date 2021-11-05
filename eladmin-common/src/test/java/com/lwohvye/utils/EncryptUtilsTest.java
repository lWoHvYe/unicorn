package com.lwohvye.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static com.lwohvye.utils.EncryptUtils.*;

public class EncryptUtilsTest {

    /**
     * 对称加密
     */
    @Test
    public void testDesEncrypt() {
        try {
            assertEquals("fAbch4/a/zBUwpVsWS+cMw==", aesEncrypt("123456"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 对称解密
     */
    @Test
    public void testDesDecrypt() {
        try {
            assertEquals("123456", aesDecrypt("fAbch4/a/zBUwpVsWS+cMw=="));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
