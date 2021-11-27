package com.lwohvye.generate;

import com.lwohvye.domain.GenConfig;
import com.lwohvye.service.IGeneratorService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;

/**
 * @author Hongyan Wang
 * @packageName com.lwohvye.generate
 * @className GeneratorCode
 * @description
 * @date 2020/12/30 0:11
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class GeneratorCode {
    @Autowired
    private IGeneratorService generatorService;

    @Test
    @Rollback(value = false)
    @Transactional(rollbackFor = Exception.class)
    public void generator() {
//        表名称，支持多表
        var tableNames = Arrays.asList("sys_resource");
        tableNames.forEach(tableName -> {
//              拿参数
            var columns = generatorService.getColumns(tableName);
            //  只生成后端的话，只需要配置下包名和是否覆盖，
            var genConfig = new GenConfig()
//                  未设置id无法生成
                    .setId(1L)
                    .setTableName(tableName)
                    .setApiAlias("资源")
                    .setModuleName("eladmin-system")
//                  根据需求更改包路径
                    .setPack("com.lwohvye.modules.system")
//                  前端路径。不生成前端可置空
                    .setPath("")
//                  作者
                    .setAuthor("Super idol lv")
//                  表前缀。生成实体时，会移除该前缀
                    .setPrefix("sys_")
//                  若文件存在，是否进行覆盖
                    .setCover(false);

//          生成代码
            generatorService.generator(genConfig, columns);
        });
    }
}
