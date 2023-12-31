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

import cn.hutool.core.util.IdUtil;
import cn.hutool.core.util.ObjectUtil;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StringUtils;
import net.coobird.thumbnailator.Thumbnails;

import java.io.File;
import java.io.OutputStream;

/**
 * 一个图片处理的工具，支持调整图片尺寸、添加水印、旋转图片、等比例压缩图片、在一些时候还可压缩图片（需注意工具的本意不是用来压缩的，当前只输出jpg格式，且有时压缩存在极限）
 * <a href="https://github.com/coobird/thumbnailator/wiki/Examples" />
 *
 * @date 2021/12/31 5:38 PM
 */
@Slf4j
public class ThumbnailUtils {

    /**
     * 压缩图片并返回新的路径。当下输出只有jpg格式。需注意，本工具的主要用途是调整图片尺寸、添加水印等，压缩有时达不到效果
     *
     * @param imageFile    图片文件
     * @param imgSizeLimit
     * @return /
     */
    public static String compressPic(File imageFile, Integer imgSizeLimit) {

        if (imageFile == null) throw new AssertionError();

        if (imageFile.length() >= 40 * 1024 * 1024) log.error("文件不能大于40M");

        var uuid = IdUtil.fastSimpleUUID();

        //拼接文件路径
        var filenameExtension = StringUtils.getFilenameExtension(imageFile.getName());
        var thumbnailFilePathName = imageFile.getParent() + File.separator + uuid;

        long size = imageFile.length();
//        默认0.8d，大致为原大小的1/5
        double quality = 0.8d;
        if (ObjectUtil.equal("png", filenameExtension)) {
//        png类型使用0.95d的压缩比率 大致为原大小的1/6
            quality = 0.95d;
        } else if (size > 10 * 1024 * 1024) {
//          非png类型，10M以上的图，使用0.6d的压缩比率
            quality = 0.6d;
        }

        try {
            if (ObjectUtil.equal("png", filenameExtension) && size < imgSizeLimit * 2 * 1024) {
//                png 转 jpg 大小减少较多
                Thumbnails.of(imageFile).scale(1f).outputFormat("jpg").toFile(thumbnailFilePathName);
            } else {
                Thumbnails.of(imageFile).scale(1f).outputQuality(quality).outputFormat("jpg").toFile(thumbnailFilePathName);
            }

        } catch (Exception e1) {
            log.error("操作失败" + e1.getMessage());
        }
//        压缩后的图片都是jpg格式的
        return thumbnailFilePathName + ".jpg";
    }

    /**
     * 调整图片尺寸。还支持调整图片格式、添加水印、旋转图片等。
     *
     * @param imageFile
     * @param os
     * @date 2021/12/31 5:33 PM
     */
    @SneakyThrows
    public static void resize(File imageFile, OutputStream os) {
        Thumbnails.of(imageFile)
                // .size(1024, 768)
                .scale(0.8d) // 原图片的80%尺寸
                .outputFormat("png").toOutputStream(os);
    }

}
