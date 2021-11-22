---
- **Java 16相关的部分依旧需要做**
---

**Java 17**，发布中央仓库，需要在maven的vm中配置。若不需要deploy，无需添加
```
--add-opens java.base/java.lang=ALL-UNNAMED
--add-opens java.base/java.lang.reflect=ALL-UNNAMED
--add-opens java.base/java.util=ALL-UNNAMED
--add-opens java.base/java.text=ALL-UNNAMED
--add-opens java.desktop/java.awt.font=ALL-UNNAMED
```
---
