/*
 *    Copyright (c) 2022-2023.  lWoHvYe(Hongyan Wang)
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

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.Iterator;

// 用 NIO 的多路复用套接字实现服务器，Socket监听回显
public class EchoServerNIO {
    private static final int ECHO_SERVER_PORT = 6789;
    private static final int ECHO_SERVER_TIMEOUT = 5000;
    private static final int BUFFER_SIZE = 1024;
    private static Selector selector = null;// 多路复用选择器
    private static ByteBuffer buffer = null;// 缓冲区

    public static void main(String[] args) {
        init();
        listen();
    }

    private static void init() {
        try {
            ServerSocketChannel serverChannel = ServerSocketChannel.open(); // 创建通道
            buffer = ByteBuffer.allocate(BUFFER_SIZE); // 读写缓冲区
            serverChannel.socket().bind(new InetSocketAddress(ECHO_SERVER_PORT)); // 绑定
            serverChannel.configureBlocking(false); // 通道需设置为非阻塞

            selector = Selector.open(); // 创建选择器

            serverChannel.register(selector, SelectionKey.OP_ACCEPT); // 将通道注册到选择器上
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static void listen() {
        while (true) {
            try {
                if (selector.select(ECHO_SERVER_TIMEOUT) != 0) { // 使用 select() 来监听到达的事件，它会一直阻塞直到有至少一个事件到达。
                    Iterator<SelectionKey> keyIterator = selector.selectedKeys().iterator(); // 获取到达的事件
                    while (keyIterator.hasNext()) {
                        SelectionKey key = keyIterator.next();
                        handleKey(key); // 处理事件
                        keyIterator.remove(); // 处理完后移除事件
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private static void handleKey(SelectionKey key) throws IOException {
        SocketChannel channel = null;
        try {
            if (key.isAcceptable()) {
                ServerSocketChannel serverChannel = (ServerSocketChannel) key.channel();
                // 服务器会为每个新连接创建一个 SocketChannel
                channel = serverChannel.accept();
                channel.configureBlocking(false);
                // 这个新连接主要用于从客户端读取数据
                channel.register(selector, SelectionKey.OP_READ);
            } else if (key.isReadable()) {
                channel = (SocketChannel) key.channel();
                buffer.clear();
                if (channel.read(buffer) != -1) {
                    buffer.flip(); // 读写模式切换
                    CharBuffer charBuffer = CharsetHelper.decode(buffer);
                    String msg = charBuffer.toString();
                    System.out.println("收到" + channel.getRemoteAddress() + "的消息:" + msg);
                    channel.write(CharsetHelper.encode(CharBuffer.wrap(msg)));
                    buffer.clear(); // 写完清空
                } else {
                    channel.close();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            if (channel != null) {
                channel.close();
            }
        }
    }
}
