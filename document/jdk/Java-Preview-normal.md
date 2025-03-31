## Things that are much interesting

- [JEP 457: Class-File API (Preview)](https://openjdk.org/jeps/457)
  Goals
    - Provide an accurate, complete, performant, and standard API for reading, writing, and transforming Java class
      files, which tracks the class-file specification.
    - Enable the replacement of existing uses of ASM within the JDK, eventually enabling the removal of the JDK's
      internal copy of the ASM library. We may similarly be able to remove the JDK's two custom internal class-file
      libraries.
- [OpenJDK ClassFile API and ASM (By ASM)](https://gitlab.ow2.org/asm/asm/-/issues/317978)
- [Spring - Consider ClassFile API for reading class metadata from bytecode](https://github.com/spring-projects/spring-framework/issues/33616)
  Spring Framework is reading class bytecode to collect metadata information about the application code. We are historically using a reduced ASM fork for this purpose, 
through the MetadataReaderFactory and MetadataReader types in the org.springframework.core.type.classreading package. While Spring applications have no direct exposure to this API, 
this is especially useful when parsing @Configuration classes, or looking for annotations on application code.
Java 24 introduced a new Class-File API with JEP 484 for reading and writing classes as Java bytecode. We are adopting this feature for Java 24+ applications with a new ClassFileMetadataReader implementation in spring-core. 
This should be completely transparent for applications.
