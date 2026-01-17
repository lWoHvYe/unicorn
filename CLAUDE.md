# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Unicorn is an enterprise-grade Java framework built on Spring Boot 4.x and Java 25. It's a multi-module Gradle project providing a comprehensive backend solution with features like multi-tenancy, OAuth2, read-write separation, and virtual threads support.

## Build and Development Commands

### Building the Project
```bash
# Clean build
./gradlew clean build

# Build without tests
./gradlew assemble

# Create executable JAR
./gradlew bootJar

# Build Docker image
./gradlew bootBuildImage
```

### Running Tests
```bash
# Run all tests
./gradlew test

# Run checks (includes tests)
./gradlew check

# Run module-specific tests for different Java versions
./gradlew java21Test
./gradlew java24Test
./gradlew java25Test
```

### Running the Application
```bash
# Using Gradle
./gradlew bootRun

# Using startup scripts (production)
cd script
./startup.sh      # Normal startup with ZGC
./startup_debug.sh # Debug mode on port 5005
./shutdown.sh     # Graceful shutdown
./restart.sh      # Restart application
```

### Publishing
```bash
# Publish to local Maven repository
./gradlew publishToMavenLocal

# Publish to remote repositories (requires credentials)
./gradlew publish
```

## Architecture and Module Structure

### Core Modules
- **unicorn-core**: Base classes, utilities, and multi-release JAR support (Java 17-25)
- **unicorn-beans**: Basic bean definitions and configurations
- **unicorn-sys-api**: System module base entities and APIs for service splitting
- **unicorn-security**: Authentication and authorization with Spring Security + JWT
- **unicorn-logging**: Aspect-based logging with RabbitMQ async support
- **unicorn-tp-tools-kotlin**: Third-party tools (email, S3) in Kotlin
- **unicorn-code-gen-kotlin**: High-flexibility code generator in Kotlin
- **unicorn-oauth2**: OAuth2 implementation with Authorization Server, Client, and Resource Server

### Application Modules
- **valentine-starter**: Main application starter (Gradle-based)
- **unicorn-starter**: Alternative starter (Maven-based) - separate repository

### Key Architectural Features

1. **Multi-Java Version Support**: Uses Multi-Release JAR files to support Java 17, 21, 24, and 25. Virtual threads are automatically enabled for Java 21+.

2. **Read-Write Separation**: Implements database and cache read-write separation using:
   - ShardingSphere for database read-write splitting
   - Redisson for Redis read-write separation

3. **Dynamic Queries**: Annotation-based JPA queries using Specification pattern (see `QueryHelp` in unicorn-core)

4. **Modular Design**: Modules are loosely coupled and can be used independently. Core module provides baseline functionality.

5. **Security Architecture**:
   - JWT-based authentication
   - Role-based access control
   - Interface-level permission control
   - Support for anonymous interfaces

6. **Message Queue Integration**: RabbitMQ for async processing, delayed messages, and service decoupling

### Important Configuration Files

- `gradle/libs.versions.toml`: Centralized dependency version management
- `buildSrc/src/main/kotlin/com.lwohvye.java-conventions.gradle.kts`: Shared build conventions
- Module-specific configurations in each module's `build.gradle.kts`

### Development Notes

1. **Java Module System**: Project uses JPMS with automatic module configuration for third-party libraries

2. **Preview Features**: Java preview features are enabled for virtual threads support

3. **Lombok**: Used extensively - ensure IDE support is configured

4. **MapStruct**: Used for DTO mapping - annotation processor is configured

5. **Testing**: JUnit 5 is used. Test files follow `*Test.java` naming convention

6. **Docker Support**: Docker Compose files available in `/document/docker/` for full environment setup

### Module Dependencies

When working on specific modules, be aware of these key dependencies:
- Most modules depend on `unicorn-core`
- `unicorn-security` depends on `unicorn-sys-api`
- `unicorn-logging` is optional but provides aspect-based logging
- `unicorn-tp-tools-kotlin` and `unicorn-code-gen-kotlin` are optional utility modules