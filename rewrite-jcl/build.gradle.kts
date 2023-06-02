plugins {
    id("org.openrewrite.build.language-library")
}

group = "org.openrewrite"
description = "Rewrite support for Job Control Language (JCL)"

// run manually with -x compileKotlin when you need to regenerate
tasks.register<JavaExec>("generateAntlrSources") {
    mainClass.set("org.antlr.v4.Tool")
    args = listOf(
        "-o", "src/main/java/org/openrewrite/jcl/internal/grammar",
        "-package", "org.openrewrite.jcl.internal.grammar",
        "-visitor"
    ) + fileTree("src/main/antlr").matching { include("**/*.g4") }.map { it.path }

    classpath = sourceSets["main"].runtimeClasspath
}

sourceSets {
    create("model") {
        compileClasspath += sourceSets.main.get().output
        runtimeClasspath += sourceSets.main.get().output
    }
}

val latest = if (project.hasProperty("releasing")) {
    "latest.release"
} else {
    "latest.integration"
}
dependencies {
    annotationProcessor("org.projectlombok:lombok:latest.release")

    compileOnly("org.projectlombok:lombok:latest.release")
    compileOnly("com.google.code.findbugs:jsr305:latest.release")
    compileOnly("org.openrewrite:rewrite-test")

    runtimeOnly("org.openrewrite.tools:java-object-diff:latest.release")

    implementation(platform("org.openrewrite:rewrite-bom:${latest}"))
    implementation("org.openrewrite:rewrite-core")
    implementation("org.antlr:antlr4:4.11.1")
    implementation("io.micrometer:micrometer-core:1.9.+")

    testImplementation("org.assertj:assertj-core:latest.release")
    testImplementation("org.junit.jupiter:junit-jupiter-api:latest.release")
    testImplementation("org.junit.jupiter:junit-jupiter-params:latest.release")
    testImplementation("org.junit-pioneer:junit-pioneer:2.0.0")
    testImplementation("org.openrewrite:rewrite-test")

    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:latest.release")
}

tasks.getByName<Test>("test") {
    useJUnitPlatform()
}
