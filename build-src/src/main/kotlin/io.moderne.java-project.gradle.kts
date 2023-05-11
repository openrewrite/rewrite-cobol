import nebula.plugin.contacts.Contact
import org.gradle.api.tasks.compile.JavaCompile
import org.gradle.api.tasks.testing.Test
import org.gradle.jvm.toolchain.JavaLanguageVersion
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile
import nebula.plugin.contacts.ContactsExtension

plugins {
    `java-base`
    kotlin("jvm")
    id("io.moderne.base")
    id("nebula.release")

    id("nebula.maven-manifest")
    id("nebula.maven-nebula-publish")
    id("nebula.maven-resolved-dependencies")

    id("nebula.contacts")
    id("nebula.info")

    id("nebula.javadoc-jar")
    id("nebula.source-jar")
    id("com.google.cloud.artifactregistry.gradle-plugin")
    id("com.jfrog.artifactory")
}
apply(plugin = "nebula.publish-verification")

repositories {
    maven {
        name = "gcp"
        url = uri("artifactregistry://us-west1-maven.pkg.dev/moderne-dev/moderne-releases")
    }
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

tasks.withType<KotlinCompile>().configureEach {
    kotlinOptions {
        jvmTarget = "1.8"
    }
}

tasks.named<JavaCompile>("compileJava") {
    options.release.set(8)
    sourceCompatibility = "1.8"
    targetCompatibility = "1.8"
}

tasks.withType<JavaCompile>().configureEach {
    options.encoding = "UTF-8"
    options.compilerArgs.add("-parameters")
    options.isFork = true
}

tasks.withType<Javadoc>().configureEach {
    isVerbose = false

    options {
        this as CoreJavadocOptions
        addStringOption("Xdoclint:none", "-quiet")
        encoding("UTF-8")
    }
}

configure<ContactsExtension> {
    val j = Contact("team@moderne.io")
    j.moniker("Team Moderne")
    people["team@moderne.io"] = j
}

tasks.named<Test>("test").configure {
    maxParallelForks = (Runtime.getRuntime().availableProcessors() / 2).takeIf { it > 0 } ?: 1
    useJUnitPlatform {
        excludeTags("debug")
    }
    jvmArgs = listOf(
        "-XX:+UnlockDiagnosticVMOptions",
        "-XX:+ShowHiddenFrames"
    )
    testLogging {
        showExceptions = true
        exceptionFormat = org.gradle.api.tasks.testing.logging.TestExceptionFormat.FULL
        showCauses = true
        showStackTraces = true
    }
}

configure<PublishingExtension> {
    publications {
        named("nebula", MavenPublication::class.java) {
            suppressPomMetadataWarningsFor("runtimeElements")
        }
    }
}

configurations.all {
    exclude("com.google.errorprone", "*")
    resolutionStrategy {
        cacheChangingModulesFor(0, TimeUnit.SECONDS)
        cacheDynamicVersionsFor(0, TimeUnit.SECONDS)
    }
}
