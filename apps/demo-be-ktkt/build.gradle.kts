import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
  application
  kotlin("jvm") version "2.1.21"
  kotlin("plugin.serialization") version "2.1.21"
  id("io.ktor.plugin") version "3.1.2"
  id("org.jetbrains.kotlinx.kover") version "0.9.1"
  id("io.gitlab.arturbosch.detekt") version "1.23.8"
  id("com.ncorti.ktfmt.gradle") version "0.22.0"
}

val ktorVersion = "3.1.2"
val exposedVersion = "0.59.0"
val koinVersion = "4.0.2"
val cucumberVersion = "7.22.0"
val junitVersion = "5.11.4"
val logbackVersion = "1.5.18"
val postgresDriverVersion = "42.7.5"
val sqliteVersion = "3.49.1.0"
val jbcryptVersion = "0.4"
val javaJwtVersion = "4.4.0"

group = "com.organiclever"

version = "0.0.1"

application { mainClass.set("com.organiclever.demoktkt.ApplicationKt") }

ktor { fatJar { archiveFileName.set("demo-be-ktkt-all.jar") } }

java {
  sourceCompatibility = JavaVersion.VERSION_21
  targetCompatibility = JavaVersion.VERSION_21
}

tasks.withType<KotlinCompile> {
  compilerOptions {
    jvmTarget.set(org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_21)
    freeCompilerArgs.add("-Xjsr305=strict")
  }
}

repositories { mavenCentral() }

dependencies {
  // Ktor server
  implementation("io.ktor:ktor-server-core:$ktorVersion")
  implementation("io.ktor:ktor-server-netty:$ktorVersion")
  implementation("io.ktor:ktor-server-content-negotiation:$ktorVersion")
  implementation("io.ktor:ktor-serialization-kotlinx-json:$ktorVersion")
  implementation("io.ktor:ktor-server-auth:$ktorVersion")
  implementation("io.ktor:ktor-server-auth-jwt:$ktorVersion")
  implementation("io.ktor:ktor-server-status-pages:$ktorVersion")
  implementation("io.ktor:ktor-server-call-logging:$ktorVersion")
  implementation("io.ktor:ktor-server-cors:$ktorVersion")

  // Database - Exposed ORM
  implementation("org.jetbrains.exposed:exposed-core:$exposedVersion")
  implementation("org.jetbrains.exposed:exposed-dao:$exposedVersion")
  implementation("org.jetbrains.exposed:exposed-jdbc:$exposedVersion")
  implementation("org.jetbrains.exposed:exposed-java-time:$exposedVersion")

  // Database drivers
  implementation("org.postgresql:postgresql:$postgresDriverVersion")

  // JWT
  implementation("com.auth0:java-jwt:$javaJwtVersion")

  // Password hashing
  implementation("org.mindrot:jbcrypt:$jbcryptVersion")

  // Dependency Injection
  implementation("io.insert-koin:koin-ktor:$koinVersion")
  implementation("io.insert-koin:koin-logger-slf4j:$koinVersion")

  // Logging
  implementation("ch.qos.logback:logback-classic:$logbackVersion")

  // Test dependencies
  testImplementation("io.ktor:ktor-server-test-host:$ktorVersion")
  testImplementation("org.jetbrains.kotlin:kotlin-test:2.1.21")
  testImplementation("org.junit.jupiter:junit-jupiter:$junitVersion")
  testImplementation("org.junit.platform:junit-platform-suite:1.11.4")
  testImplementation("io.cucumber:cucumber-java:$cucumberVersion")
  testImplementation("io.cucumber:cucumber-junit-platform-engine:$cucumberVersion")
  testImplementation("io.insert-koin:koin-test:$koinVersion")
  testImplementation("org.xerial:sqlite-jdbc:$sqliteVersion")
  testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:$junitVersion")
}

tasks.test {
  useJUnitPlatform()
  systemProperty("cucumber.junit-platform.naming-strategy", "long")
  // Sequential execution prevents race conditions during binary result writes
  maxParallelForks = 1
  // Disable standard Gradle test reports to avoid binary store EOF errors
  // with large Cucumber suites (Gradle TestOutputStore.Reader truncation issue)
  reports.junitXml.required.set(false)
  reports.html.required.set(false)
}

// Copy Gherkin specs into test resources classpath
tasks.processTestResources {
  from("${rootProject.projectDir}/../../specs/apps/demo-be/gherkin") {
    into("specs/apps/demo-be/gherkin")
  }
}

// Kover configuration
kover {
  reports {
    filters {
      excludes {
        // Exclude Exposed production DB repositories (untestable without real DB in integration
        // tests)
        classes(
          "com.organiclever.demoktkt.infrastructure.Exposed*",
          "com.organiclever.demoktkt.infrastructure.DatabaseFactory",
          "com.organiclever.demoktkt.infrastructure.tables.*",
          // Exclude main entry point (only calls embeddedServer)
          "com.organiclever.demoktkt.ApplicationKt",
          // Exclude DI module setup (wires Exposed repos, not testable without DB)
          "com.organiclever.demoktkt.plugins.DIKt",
        )
      }
    }
    total {
      xml {
        onCheck = false
        xmlFile.set(file("build/reports/kover/report.xml"))
      }
      verify { rule { minBound(90) } }
    }
  }
}

// detekt configuration
detekt {
  config.setFrom(files("detekt.yml"))
  buildUponDefaultConfig = true
  allRules = false
}

// ktfmt configuration
ktfmt { googleStyle() }
