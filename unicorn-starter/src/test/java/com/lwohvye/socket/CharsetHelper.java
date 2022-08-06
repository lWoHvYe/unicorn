/*
 *    Copyright (c) 2022.  lWoHvYe(Hongyan Wang)
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

package com.lwohvye.socket;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.*;

public final class CharsetHelper {
    private static final String UTF_8 = "UTF-8";
    private static final CharsetEncoder encoder = StandardCharsets.UTF_8.newEncoder();
    private static final CharsetDecoder decoder = StandardCharsets.UTF_8.newDecoder();

    private CharsetHelper() {
    }

    public static ByteBuffer encode(CharBuffer in) throws CharacterCodingException {
        return encoder.encode(in);
    }

    public static CharBuffer decode(ByteBuffer in) throws CharacterCodingException {
        return decoder.decode(in);
    }
}
