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
        withGradle() {
            tasks: ['clean', 'build'],
            switches: ['--stacktrace']
        }

      }
    }

  }
}
