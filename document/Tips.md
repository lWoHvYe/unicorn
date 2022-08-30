#### Profiles说明
- dev/prod 略
- jigsaw 用于Server Jar Package时外置依赖
- github-pkg 指定distrubutionRepo，用于 Github Package的 Job
- ossrh-mc 指定distrubutionRepo，用于Release到中央仓库
- 另还可在Profile中配置dependency，在active后就会引入依赖，这里在starter模块，默认是单表模式，在active jigsaw后就引入sharding-jdbc分库分表
