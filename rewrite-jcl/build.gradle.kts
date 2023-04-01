plugins {
    `java-library`
    id("io.moderne.java-project")
}

description = "Rewrite support for the JCL language"

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

    implementation(platform("org.openrewrite.recipe:rewrite-recipe-bom:${latest}"))
    implementation("org.openrewrite:rewrite-core")
    implementation("org.antlr:antlr4:4.11.1")
    implementation("io.micrometer:micrometer-core:1.9.+")

    testImplementation("org.junit.jupiter:junit-jupiter-api:latest.release")
    testImplementation("org.junit.jupiter:junit-jupiter-params:latest.release")
    testImplementation("org.openrewrite:rewrite-test")
    testImplementation("io.moderne:moderne-ast-write:${latest}")

    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:latest.release")
}

tasks.getByName<Test>("test") {
    useJUnitPlatform()
}