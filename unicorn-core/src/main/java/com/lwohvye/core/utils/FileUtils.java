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

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IoUtil;
import cn.hutool.core.map.MapUtil;
import cn.hutool.core.util.IdUtil;
import cn.hutool.poi.excel.BigExcelWriter;
import cn.hutool.poi.excel.ExcelUtil;
import com.lwohvye.core.exception.BadRequestException;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.util.IOUtils;
import org.apache.poi.xssf.streaming.SXSSFSheet;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.*;
import java.nio.channels.FileChannel;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.MessageDigest;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.time.LocalTime;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * File工具类，扩展 hutool 工具包
 *
 * @author Zheng Jie
 * @date 2018-12-27
 */
@Slf4j
public class FileUtils extends FileUtil {

    /**
     * 系统临时目录
     * <br>
     * windows 包含路径分割符，但Linux 不包含,
     * 在windows \\==\ 前提下，
     * 为安全起见 同意拼装 路径分割符，
     * <pre>
     *       java.io.tmpdir
     *       windows : C:\Users/xxx\AppData\Local\Temp\
     *       linux: /temp
     * </pre>
     */
    public static final String SYS_TEM_DIR = System.getProperty("java.io.tmpdir") + File.separator;
    /**
     * 定义GB的计算常量
     */
    private static final int GB = 1024 * 1024 * 1024;
    /**
     * 定义MB的计算常量
     */
    private static final int MB = 1024 * 1024;
    /**
     * 定义KB的计算常量
     */
    private static final int KB = 1024;

    /**
     * 格式化小数
     */
    private static final DecimalFormat DF = new DecimalFormat("0.00");

    public static final String IMAGE = "图片";
    public static final String TXT = "文档";
    public static final String MUSIC = "音乐";
    public static final String VIDEO = "视频";
    public static final String OTHER = "其他";


    /**
     * MultipartFile转File
     */
    public static File toFile(MultipartFile multipartFile) {
        // 获取文件名
        String fileName = multipartFile.getOriginalFilename();
        // 获取文件后缀
        String prefix = "." + getExtensionName(fileName);
        File dest = null;
        try {
            // 用uuid作为文件名，防止生成的临时文件重复
            dest = new File(SYS_TEM_DIR + IdUtil.simpleUUID() + prefix);
            // MultipartFile to File
            multipartFile.transferTo(dest);
        } catch (IOException e) {
            log.error(e.getMessage(), e);
        }
        return dest;
    }

    /**
     * 获取文件扩展名，不带 .
     */
    public static String getExtensionName(String filename) {
        if ((filename != null) && (!filename.isEmpty())) {
            int dot = filename.lastIndexOf('.');
            if ((dot > -1) && (dot < (filename.length() - 1)))
                return filename.substring(dot + 1).replaceAll("[/\\\\]", "");
        }
        // 若不含扩展名，还是试着移除下路径相关的 / 或 \ 。
        return Objects.requireNonNull(filename).replaceAll("[/\\\\]", "");
    }

    /**
     * Java文件操作 获取不带扩展名的文件名
     */
    public static String getFileNameNoEx(String filename) {
        if ((filename != null) && (!filename.isEmpty())) {
            int dot = filename.lastIndexOf('.');
            if (dot > -1)
                return filename.substring(0, dot).replaceAll("[./\\\\]", "");
        }
        // 移除文件命中的 . / 这些跟目录层级有关的部分
        return Objects.requireNonNull(filename).replaceAll("[./\\\\]", "");
    }

    /**
     * 文件大小转换
     */
    public static String getSize(long size) {
        String resultSize;
        if (size / GB >= 1) {
            //如果当前Byte的值大于等于1GB
            resultSize = DF.format(size / (float) GB) + "GB   ";
        } else if (size / MB >= 1) {
            //如果当前Byte的值大于等于1MB
            resultSize = DF.format(size / (float) MB) + "MB   ";
        } else if (size / KB >= 1) {
            //如果当前Byte的值大于等于1KB
            resultSize = DF.format(size / (float) KB) + "KB   ";
        } else {
            resultSize = size + "B   ";
        }
        return resultSize;
    }

    /**
     * inputStream 转 File
     */
    static File inputStreamToFile(InputStream ins, String name) {
        try {
            var path = Path.of(SYS_TEM_DIR + name);
            Files.copy(ins, path);
            return path.toFile();
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        } finally {
            CloseUtils.close(ins);
        }
    }

    /**
     * 将文件名解析成文件的上传路径
     */
    public static File upload(MultipartFile multipartFile, String filePath) {
        Date date = new Date();
        SimpleDateFormat format = new SimpleDateFormat("yyyyMMddhhmmssS");
        String name = getFileNameNoEx(verifyFilename(multipartFile.getOriginalFilename()));
        String suffix = getExtensionName(multipartFile.getOriginalFilename());
        String nowStr = "-" + format.format(date);
        try {
            String fileName = name + nowStr + "." + suffix;
            String path = filePath + fileName;
            // getCanonicalFile 可解析正确各种路径
            File dest = new File(path).getCanonicalFile();
            // 检测是否存在目录
            if (!dest.getParentFile().exists() && !dest.getParentFile().mkdirs()) {
                log.warn("upload was not successful.");
            }
            // 文件写入
            multipartFile.transferTo(dest);
            return dest;
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
        return null;
    }

    /**
     * 下载文件
     *
     * @param request  /
     * @param response /
     * @param file     /
     */
    public static void downloadFile(HttpServletRequest request, HttpServletResponse response, File file, boolean deleteOnExit) {
        response.setCharacterEncoding(request.getCharacterEncoding());
        response.setContentType("application/octet-stream");
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(file);
            response.setHeader("Content-Disposition", "attachment; filename=" + file.getName());
            IOUtils.copy(fis, response.getOutputStream());
            response.flushBuffer();
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                    if (deleteOnExit) {
                        file.deleteOnExit();
                    }
                } catch (IOException e) {
                    log.error(e.getMessage(), e);
                }
            }
        }
    }

    /**
     * 导出excel
     */
    public static void downloadExcel(List<Map<String, Object>> list, HttpServletResponse response) throws IOException {
        String tempPath = SYS_TEM_DIR + IdUtil.fastSimpleUUID() + ".xlsx";
        File file = new File(tempPath);
        BigExcelWriter writer = ExcelUtil.getBigWriter(file);
        // 一次性写出内容，使用默认样式，强制输出标题
        writer.write(list, true);
        SXSSFSheet sheet = (SXSSFSheet) writer.getSheet();
        //上面需要强转SXSSFSheet  不然没有trackAllColumnsForAutoSizing方法
        sheet.trackAllColumnsForAutoSizing();
        //列宽自适应
        writer.autoSizeColumnAll();
        //response为HttpServletResponse对象
        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;charset=utf-8");
        //test.xls是弹出下载对话框的文件名，不能为中文，中文请自行编码
        response.setHeader("Content-Disposition", "attachment;filename=file.xlsx");
        ServletOutputStream out = response.getOutputStream();
        // 终止后删除临时文件
        file.deleteOnExit();
        writer.flush(out, true);
        //此处记得关闭输出Servlet流
        IoUtil.close(out);
    }

    /**
     * 通过bean生成excel
     *
     * @param aliasMap 属性与标题的映射，要保证顺序性可使用LinkedHashMap
     * @param tList    集合
     * @param temPath  导出的目录
     * @param title    文件标题
     * @date 2022/3/29 7:51 PM
     */
    public static <T> String createExcel(Map<String, String> aliasMap, @NonNull List<T> tList, String temPath, @NonNull String title) {
        temPath = StringUtils.hasText(temPath) ? temPath + File.separator : SYS_TEM_DIR;
        var fileName = DateUtil.today() + " " + LocalTime.now().toString().replace(":", "_").substring(0, 8) + "-" + title + ".xlsx";
        var writer = ExcelUtil.getBigWriter(temPath + fileName);
        if (MapUtil.isNotEmpty(aliasMap)) {
            // 自定义标题别名
            aliasMap.forEach(writer::addHeaderAlias);
            // 默认的，未添加alias的属性也会写出，如果想只写出加了别名的字段，可以调用此方法排除之
            writer.setOnlyAlias(true);

            // 合并单元格后的标题行，使用默认标题样式
            writer.merge(aliasMap.size() - 1, title);
        }
        // 一次性写出内容，使用默认样式
        writer.write(tList);
        // 关闭writer，释放内存
        writer.close();
        return fileName;
    }

    public static String getFileType(String type) {
        String documents = "txt doc pdf ppt pps xlsx xls docx";
        String music = "mp3 wav wma mpa ram ra aac aif m4a";
        String video = "avi mpg mpe mpeg asf wmv mov qt rm mp4 flv m4v webm ogv ogg";
        String image = "bmp dib pcp dif wmf gif jpg tif eps psd cdr iff tga pcd mpt png jpeg";
        if (image.contains(type)) {
            return IMAGE;
        } else if (documents.contains(type)) {
            return TXT;
        } else if (music.contains(type)) {
            return MUSIC;
        } else if (video.contains(type)) {
            return VIDEO;
        } else {
            return OTHER;
        }
    }

    public static void checkSize(long maxSize, long size) {
        // 1M
        int len = 1024 * 1024;
        if (size > (maxSize * len)) {
            throw new BadRequestException("文件超出规定大小");
        }
    }

    /**
     * 判断两个文件是否相同
     */
    public static boolean check(File file1, File file2) {
        String img1Md5 = getMd5(file1);
        String img2Md5 = getMd5(file2);
        if (img1Md5 != null) {
            return img1Md5.equals(img2Md5);
        }
        return false;
    }

    /**
     * 判断两个文件是否相同
     */
    public static boolean check(String file1Md5, String file2Md5) {
        return file1Md5.equals(file2Md5);
    }

    private static byte[] getByte(File file) {
        // 得到文件长度
        byte[] b = new byte[(int) file.length()];
        try (var in = new FileInputStream(file)) {
            in.read(b);
        } catch (IOException e) {
            log.error(e.getMessage(), e);
            return new byte[0];
        }
        return b;
    }

    public static String getMd5(File file) {
        return getMd5(getByte(file));
    }

    private static String getMd5(byte[] bytes) {
        // 16进制字符
        char[] hexDigits = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
        try {
            MessageDigest mdTemp = MessageDigest.getInstance("MD5");
            mdTemp.update(bytes);
            byte[] md = mdTemp.digest();
            int j = md.length;
            char[] str = new char[j * 2];
            int k = 0;
            // 移位 输出字符串
            for (byte byte0 : md) {
                str[k++] = hexDigits[byte0 >>> 4 & 0xf];
                str[k++] = hexDigits[byte0 & 0xf];
            }
            return new String(str);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
        return null;
    }

    /**
     * 验证并过滤非法的文件名
     *
     * @param fileName 文件名
     * @return 文件名
     */
    @Nullable
    public static String verifyFilename(String fileName) {
        if (Objects.isNull(fileName))
            return null;
        // 过滤掉特殊字符
        fileName = fileName.replaceAll("[\\\\/:*?\"<>|~\\s]", "");

        // 去掉文件名开头和结尾的空格和点
        fileName = fileName.trim().replaceAll("^[. ]+|[. ]+$", "");

        // 不允许文件名超过255（在Mac和Linux中）或260（在Windows中）个字符
        int maxFileNameLength = 255;
        if (System.getProperty("os.name").startsWith("Windows")) {
            maxFileNameLength = 260;
        }
        if (fileName.length() > maxFileNameLength) {
            fileName = fileName.substring(0, maxFileNameLength);
        }

        // 过滤掉控制字符
        fileName = fileName.replaceAll("[\\p{Cntrl}]", "");

        // 过滤掉 ".." 路径
        fileName = fileName.replaceAll("\\.{2,}", "");

        // 去掉文件名开头的 ".."
        fileName = fileName.replaceAll("^\\.+/", "");

        // 保留文件名中最后一个 "." 字符，过滤掉其他 "."
        fileName = fileName.replaceAll("^(.*)(\\.[^.]*)$", "$1").replaceAll("\\.", "") +
                fileName.replaceAll("^(.*)(\\.[^.]*)$", "$2");

        return fileName;
    }

    /**
     * 传统文件拷贝方式
     *
     * @param source
     * @param target
     * @date 2022/2/14 3:16 PM
     */
    public static void fileCopy(String source, String target) throws IOException {
        // 传统方式
        // try (FileInputStream in = new FileInputStream(source)) {
        //     try (FileOutputStream out = new FileOutputStream(target)) {
        //          Java 9开始的方式，在本身就是流时，用下面这种更简洁一些，否则还是用Files.copy吧
        //          This method may block indefinitely reading from the input stream, or writing to the output stream. 这个是block的
        //          in.transferTo(out);
        //          原始方式
        //         var bytes = new byte[1024];
        //         int bytesRead;
        //         while ((bytesRead = in.read(bytes)) != -1)
        //             out.write(bytes, 0, bytesRead);
        //     }
        // }
        // 7开始，新的方式
        Files.copy(Paths.get(source), Paths.get(target));
    }

    /**
     * NIO 拷贝文件
     *
     * @param source
     * @param target
     * @date 2022/1/26 3:57 PM
     */
    public static void fileCopyNIO(String source, String target) throws IOException {
        try (var in = new FileInputStream(source)) {
            try (var out = new FileOutputStream(target)) {
                var inChannel = in.getChannel();
                var outChannel = out.getChannel();

                // 带缓冲区的拷贝
                // var buffer = ByteBuffer.allocate(4096);
                // while (inChannel.read(buffer) != -1) {
                //     buffer.flip(); // 读写模式切换
                //     outChannel.write(buffer);
                //     buffer.clear(); // 写完清空 + 为下一轮的读操作作准备
                // }
                // FileChannel的transferTo()/transferFrom()，底层就是sendfile() 系统调用函数。Kafka 这个开源项目就用到它，其为什么这么快，就可以提到零拷贝sendfile这个点。
                // 拓展，零拷贝实现的几种方式
                // 另一种方式。比上面的简洁很多
                outChannel.transferFrom(inChannel, 0, inChannel.size());
                // 另一种写法
                // inChannel.transferTo(0, inChannel.size(), outChannel);
            }
        }
    }

    /**
     * NIO遍历目录下所有文件（包括子目录）
     *
     * @param source
     * @date 2022/1/26 4:20 PM
     */
    public static void listFiles(String source) throws IOException {
        var initPath = Paths.get(source);
        // 通过重写FileVisitor中部分方法的逻辑来实现
        Files.walkFileTree(initPath, new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                System.out.println(file.getFileName().toString());
                // return super.visitFile(file, attrs);
                return FileVisitResult.CONTINUE;
            }
        });
    }

    /**
     * mmap内存映射文件，针对小文件效率还可以，也可针对大文件做部分映射
     *
     * @param source
     * @date 2022/2/7 12:27 PM
     */
    @SneakyThrows
    public static void mappedFile(String source) {
        var path = Paths.get(source);
        try (var fileChannel = FileChannel.open(path)) {
            // try (var fileChannel = new RandomAccessFile(source, "rw").getChannel()) {
            // var size = fileChannel.size();
            var size = 0x8000000; //128 Mb
            // MappedByteBuffer是 ByteBuffer 的子类，也可以使用其中的部分操作
            var mmap = fileChannel.map(FileChannel.MapMode.READ_WRITE, 0, size); // 将文件的前size个字节映射到内存
            // for (int i = 0; i < size; i++) {
            //     var src = mmap.get(i); // 获取指定位置字节。随机读
            //     mmap.put(i, (byte) (src + 1)); // 修改
            // }
            while (mmap.hasRemaining()) { // 顺序读
                var src = mmap.get(); // 获取当前位置字节
                mmap.put((byte) (src + 2)); // 修改当前位置字节
            }
            mmap.force(); // 强制输出改动到文件
            mmap.clear();

            // 下面是在指定位置读写的示例
            {
                // 写
                var data = new byte[4];
                var position = 8;
                // 从当前 mmap 指针的位置写入 4b 的数据
                mmap.put(data);
                // 指定 position 写入 4b 的数据
                var subBuffer = mmap.slice();
                subBuffer.position(position);
                subBuffer.put(data);
            }
            {
                // 读
                var data = new byte[4];
                var position = 8;
                // 从当前 mmap 指针的位置读取 4b 的数据
                mmap.get(data);
                // 指定 position 读取 4b 的数据
                var subBuffer = mmap.slice();
                subBuffer.position(position);
                subBuffer.get(data);
            }
            {
                // FileChannel锁相关
                var lock = fileChannel.lock(); //调用lock，阻塞
                lock.release(); // 释放锁
                var lock1 = fileChannel.tryLock(); //调用tryLock，立即响应，加锁失败返回null
                if (Objects.nonNull(lock1)) lock1.close(); // 释放锁，底层调用的release()

                try (var lock2 = fileChannel.lock()) { // try-with-resources。快捷键 资源.twr
                    // doSomething
                }
            }
        }
    }
}
