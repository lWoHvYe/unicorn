- **Java 16相关的部分依旧需要做**

**Java 17**，发布中央仓库，需要在maven的vm中配置。若不需要deploy，无需添加

```
--add-opens java.base/java.lang=ALL-UNNAMED
--add-opens java.base/java.lang.reflect=ALL-UNNAMED
--add-opens java.base/java.util=ALL-UNNAMED
--add-opens java.base/java.text=ALL-UNNAMED
--add-opens java.desktop/java.awt.font=ALL-UNNAMED
```

#### ZGC

[ZGC](https://wiki.openjdk.java.net/display/zgc/Main)

```shell
java -XX:+UseZGC 
```

**What does the "Z" in ZGC stand for?**

It doesn't stand for anything, ZGC is just a name. It was originally inspired by, or a homage to, ZFS (the filesystem) which in many ways was revolutionary when it first came out. Originally, ZFS was an acronym for "Zettabyte File System", but that meaning was abandoned and it was later said to not stand for anything. It's just a name. See Jeff Bonwick's Blog for more details.

**Is it pronounced "zed gee see" or "zee gee see"?**

There's no preferred pronunciation, both are fine.

注：VisualVM的Visual GC插件尚不支持ZGC的VM

#### Project Loom

[Project Loom](https://wiki.openjdk.java.net/display/loom)


