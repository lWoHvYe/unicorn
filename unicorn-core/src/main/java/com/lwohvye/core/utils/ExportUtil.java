/*
 *    Copyright (c) 2025.  lWoHvYe(Hongyan Wang)
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

import javax.sql.DataSource;
import java.io.*;
import java.net.URL;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import java.util.zip.Deflater;

public class ExportUtil {
    private final DataSource dataSource;

    public ExportUtil(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    // 数据库查询方法，针对大数据量，分页查询处理。这里只是示例
    private List<DataRecord> queryDataFromDB(String query) throws SQLException {
        List<DataRecord> records = new ArrayList<>();
        try (var conn = dataSource.getConnection();
             var stmt = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
            stmt.setFetchSize(1000); // 流式读取设置
            var rs = stmt.executeQuery(query);
            while (rs.next()) {
                records.add(new DataRecord(
                        rs.getString("id"),
                        rs.getString("imageUrl"),
                        rs.getTimestamp("createTime")
                ));
            }
        }
        return records;
    }

    // 生成Excel文件，采用流式写入，减少内存占用
    public static File generateTempExcel(List<Map<String, Object>> data) throws IOException {
        var excelFile = File.createTempFile("export_", ".xlsx");
        try (var workbook = new SXSSFWorkbook(1000)) { // 在close前是可以不断写入的，所以大数据量下可以与查询结合起来，比如一次查1000条、处理完再查询
            var sheet = workbook.createSheet();

            // 创建标题行（取第一个Map的keySet）
            if (!data.isEmpty()) {
                var headerRow = sheet.createRow(0);
                int colIdx = 0;
                for (var key : data.get(0).keySet()) {
                    var cell = headerRow.createCell(colIdx++);
                    cell.setCellValue(key);
                }
            }

            // 填充数据行
            for (int i = 0; i < data.size(); i++) {
                var row = sheet.createRow(i + 1);
                var rowData = data.get(i);
                int colIdx = 0;

                for (var value : rowData.values()) {
                    var cell = row.createCell(colIdx++);
                    setCellValue(cell, value);
                }

                if (i % 1000 == 0) {
                    sheet.flushRows(1000);
                }
            }
            workbook.write(new FileOutputStream(excelFile));
        }
        return excelFile;
    }

    private static void setCellValue(Cell cell, Object value) {
        switch (value) {
            case null -> cell.setCellValue("");
            case Number number -> cell.setCellValue(number.doubleValue());
            case Date date -> cell.setCellValue(date);
            case Boolean b -> cell.setCellValue(b);
            default -> cell.setCellValue(value.toString());
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
