pipeline {
  agent any
  stages {
    stage('Checkout') {
      steps {
        git(url: 'https://github.com/lWoHvYe/unicorn.git', branch: 'main')
      }
    }

    stage('Gradle Build') {
      steps {
        script {
          def initScript = """
          allprojects {
            repositories {
              maven { url 'https://maven.aliyun.com/repository/public' }
              maven { url 'https://maven.aliyun.com/repository/google' }
            }
          }
          """
          writeFile file: 'init.gradle', text: initScript
        }
        withGradle() {
          sh './gradlew clean build --init-script init.gradle'
        }

      }
    }

  }
}
