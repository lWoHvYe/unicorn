/*
 *    Copyright (c) 2025-2026.  lWoHvYe(Hongyan Wang)
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

import net.coobird.thumbnailator.Thumbnails;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import javax.sql.DataSource;
import java.io.*;
import java.net.URL;
import java.sql.*;
import java.sql.Date;
import java.util.*;
import java.util.function.Function;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import java.util.zip.Deflater;

public class ExportUtil {
    private final DataSource dataSource;

    public ExportUtil(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    // 数据库查询方法，针对大数据量，分页查询处理。这里只是示例
    public void queryDataFromDB(String query, List<String> headers, FileOutputStream fos) throws SQLException, IOException {
        try (var conn = dataSource.getConnection();
             var stmt = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
             var workbook = new SXSSFWorkbook(2000)) {
            var sheet = workbook.createSheet();
            int rowNum = 0;
            var headerRow = sheet.createRow(rowNum++);
            int colNum = 0;
            for (var header : headers) {
                var cell = headerRow.createCell(colNum++);
                cell.setCellValue(header);
            }
            stmt.setFetchSize(1000); // 流式读取设置
            var rs = stmt.executeQuery(query);
            while (rs.next()) {
                var row = sheet.createRow(rowNum++);
                colNum = 0;
                for (var header : headers) {
                    var cell = row.createCell(colNum++);
                    setCellValue(cell, rs.getObject(header));
                }
                if (rowNum % 1000 == 0)
                    sheet.flushRows(1000);
            }
            workbook.write(fos);
        }
    }

    // 生成Excel文件，采用流式写入，减少内存占用
    public static <R extends List<Map<String, Object>>> File generateTempExcel(Function<Pageable, R> fetchDataFunc) throws IOException {
        var excelFile = File.createTempFile("export_", ".xlsx");
        try (var workbook = new SXSSFWorkbook(2000)) { // 在close前是可以不断写入的，所以大数据量下可以与查询结合起来，比如一次查1000条、处理完再查询
            var sheet = workbook.createSheet();
            var pageRequest = PageRequest.of(1, 1000);
            var data = fetchDataFunc.apply(pageRequest);
            // 创建标题行（取第一个Map的keySet）
            if (!data.isEmpty()) {
                var headerRow = sheet.createRow(0);
                int colIdx = 0;
                for (var key : data.get(0).keySet()) {
                    var cell = headerRow.createCell(colIdx++);
                    cell.setCellValue(key);
                }
            }
            int rowNum = 1;
            while (!data.isEmpty()) {
                // 填充数据行
                for (Map<String, Object> rowData : data) {
                    var row = sheet.createRow(++rowNum);
                    int colIdx = 0;
                    for (var value : rowData.values()) {
                        var cell = row.createCell(colIdx++);
                        setCellValue(cell, value);
                    }
                }
                sheet.flushRows(1000);
                pageRequest = pageRequest.next();
                data = fetchDataFunc.apply(pageRequest);
            }
            workbook.write(new FileOutputStream(excelFile));
        }
        return excelFile;
    }

    private static void setCellValue(Cell cell, Object value) {
        if (Objects.isNull(value))
            cell.setCellValue("");
        else if (value instanceof Number number)
            cell.setCellValue(number.doubleValue());
        else if (value instanceof Boolean b)
            cell.setCellValue(b);
        else if (value instanceof Date d)
            cell.setCellValue(d);
        else
            cell.setCellValue(value.toString());
    }

    private void mergeFile(String fileName, String filePrefix, int fileCount) throws IOException {
        try (var fos = new FileOutputStream(fileName)) {
            for (int i = 0; i < fileCount; i++) {
                try (var fis = new FileInputStream(filePrefix + i + ".xlsx")) {
                    // 这里直接合并文件流，避免加载到内存
                    fis.getChannel().transferTo(0, fis.available(), fos.getChannel());
                }
            }
        }
    }

    // 压缩图片方法，大文件分卷压缩
    private File compressImages(List<DataRecord> records) throws IOException {
        var zipFile = File.createTempFile("images_", ".zip");
        try (var zos = new ZipOutputStream(new FileOutputStream(zipFile))) {
            zos.setLevel(Deflater.BEST_SPEED);
            for (int i = 0; i < records.size(); i++) {
                var record = records.get(i);
                try (var imageStream = downloadImage(record.imageUrl())) {
                    zos.putNextEntry(new ZipEntry("img_" + i + ".jpg"));
                    Thumbnails.of(imageStream) // 压缩图片
                            .scale(0.6)
                            .toOutputStream(zos);
                    if (i % 500 == 0) zos.flush(); // 刷盘
                }
            }
        }
        return zipFile;
    }

    // 辅助方法
    private InputStream downloadImage(String url) throws IOException {
        //TODO 实现具体图片下载逻辑
        return new URL(url).openStream();
    }

}

record DataRecord(String id, String imageUrl, Timestamp createTime) {
}
