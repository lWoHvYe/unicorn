#### Profiles说明
- dev/prod 略
- jigsaw 用于Server Jar Package时外置依赖，这样可以大大减小jar的size，但是变更的依赖要自己在服务器上维护，虽然理论上依赖的变更不是频繁的
- github-pkg 指定distributionRepo，用于 GitHub Package的 Job
- ossrh-mc 指定distributionRepo，用于Release到中央仓库
- 另还可在Profile中配置dependency，在active后就会引入依赖，这里在unicorn-starter模块，默认是单表模式，在active jigsaw后就引入sharding-jdbc分库分表
