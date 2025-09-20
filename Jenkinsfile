pipeline {
  agent any
  stages {
    stage('Checkout') {
      steps {
        git(url: 'https://github.com/lWoHvYe/unicorn.git', branch: 'main')
      }
    }

    stage('Gradle Build ') {
      steps {
        withGradle {
        environment {
          GRADLE_USER_HOME = "${env.WORKSPACE}/.gradle"
        }
        sh './gradlew build --add-opens java.base/java.lang=ALL-UNNAMED'
        }
      }
    }

  }
}
