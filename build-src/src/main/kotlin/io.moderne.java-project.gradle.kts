import org.gradle.api.tasks.compile.JavaCompile
import org.gradle.api.tasks.testing.Test
import org.gradle.jvm.toolchain.JavaLanguageVersion
import org.jetbrains.kotlin.gradle.tasks.KotlinCompile
import nebula.plugin.contacts.Contact
import nebula.plugin.contacts.ContactsExtension

plugins {
    `java-base`
    kotlin("jvm")
    id("io.moderne.base")
    id("nebula.release")

    id("nebula.maven-manifest")// version "17.3.2"
    id("nebula.maven-nebula-publish")// version "17.3.2"
    id("nebula.maven-resolved-dependencies")// version "17.3.2"

    id("nebula.contacts")// version "5.1.0"
    id("nebula.info")// version "9.3.0"

    id("nebula.javadoc-jar")// version "17.3.2"
    id("nebula.source-jar")// version "17.3.2"
}
apply(plugin = "nebula.publish-verification")

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

publishing {
    repositories {
        maven {
            name = "moderne"
            url = uri("https://us-west1-maven.pkg.dev/moderne-dev/moderne-recipe")
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
