import org.tmatesoft.svn.core.internal.util.jna.SVNGnomeKeyring.setPassword

plugins {
    `java-library`
    id("io.moderne.java-project")
}

description = "Rewrite support for the COBOL language"

// run manually with -x compileKotlin when you need to regenerate
tasks.register<JavaExec>("generateAntlrSources") {
    mainClass.set("org.antlr.v4.Tool")
    args = listOf(
        "-o", "src/main/java/org/openrewrite/cobol/internal/grammar",
        "-package", "org.openrewrite.cobol.internal.grammar",
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

val modelImplementation: Configuration by configurations.getting {
    extendsFrom(configurations.implementation.get())
}

val modelAnnotationProcessor: Configuration by configurations.getting
val modelCompileOnly: Configuration by configurations.getting

configurations["modelRuntimeOnly"].extendsFrom(configurations.runtimeOnly.get())

val latest = if (project.hasProperty("releasing")) {
    "latest.release"
} else {
    "latest.integration"
}

dependencies {
    compileOnly("org.projectlombok:lombok:latest.release")
    compileOnly("com.google.code.findbugs:jsr305:latest.release")
    compileOnly("org.openrewrite:rewrite-test")
    annotationProcessor("org.projectlombok:lombok:latest.release")
    implementation(platform("org.openrewrite.recipe:rewrite-recipe-bom:${latest}"))
    implementation("org.openrewrite:rewrite-core")
    implementation("org.antlr:antlr4:4.11.1")
    implementation("io.micrometer:micrometer-core:1.9.+")

    modelImplementation("org.openrewrite:rewrite-java-17")
    modelAnnotationProcessor("org.projectlombok:lombok:latest.release")
    modelCompileOnly("org.projectlombok:lombok:latest.release")
    modelImplementation("ch.qos.logback:logback-classic:latest.release")

    testImplementation(platform(kotlin("bom", "1.6.21")))
    testImplementation(kotlin("reflect"))
    testImplementation(kotlin("stdlib"))
    testImplementation("org.junit.jupiter:junit-jupiter-api:latest.release")
    testImplementation("org.junit.jupiter:junit-jupiter-params:latest.release")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:latest.release")

    testImplementation("org.openrewrite:rewrite-test")
    testImplementation("io.moderne:moderne-ast-write:${latest}")
    testImplementation("org.assertj:assertj-core:latest.release")
    testImplementation("io.github.classgraph:classgraph:latest.release")

    testImplementation("org.openrewrite:rewrite-groovy")
    testImplementation("org.openrewrite:rewrite-maven")
    testImplementation("org.openrewrite:rewrite-xml")
}

val testConf = configurations.create("test").apply {
    extendsFrom(configurations.getByName("testRuntimeClasspath"))
}

val testOutputFile = file("build/libs/${project.name}-${project.version}-test.jar")
val testJar = tasks.register<Jar>("testJar") {
    from(sourceSets.test.get().output)
    getArchiveClassifier().set("test")
}

artifacts {
    add(testConf.name, testJar)
}

artifactory {
    publish {
        setContextUrl("https://artifactory.moderne.ninja/artifactory")
        repository {
            setRepoKey("moderne-private")
            setUsername(System.getenv("AST_PUBLISH_USERNAME"))
            setPassword(System.getenv("AST_PUBLISH_PASSWORD"))
        }
        defaults {
            publications("nebula")
            setPublishArtifacts(true)
            setPublishPom(true)
        }
    }
}
