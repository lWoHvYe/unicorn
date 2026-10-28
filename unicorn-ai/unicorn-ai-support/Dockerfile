# Build from the repository root:
#   docker build -f unicorn-ai/unicorn-ai-support/Dockerfile -t unicorn-ai-support .
FROM eclipse-temurin:25-jdk AS build
WORKDIR /workspace
COPY . .
RUN chmod +x gradlew && ./gradlew :unicorn-ai-support:bootJar --no-daemon

FROM eclipse-temurin:25-jre
WORKDIR /app
COPY --from=build /workspace/unicorn-ai/unicorn-ai-support/build/libs/unicorn-ai-support-*.jar /app/app.jar
EXPOSE 8096
ENTRYPOINT ["java", "-XX:MaxRAMPercentage=75", "-jar", "/app/app.jar", "--spring.profiles.active=infra"]
