plugins {
    id("io.moderne.java-project")
    id("io.moderne.rewrite")
}

dependencies {
    implementation("org.openrewrite:rewrite-xml")
    implementation("org.openrewrite:rewrite-maven")
    implementation("org.openrewrite:rewrite-groovy")

    implementation(project(":rewrite-cobol"))
    implementation(project(":rewrite-cobol", "test"))
    implementation("io.github.classgraph:classgraph:latest.release")
    implementation("io.moderne:moderne-ast-write:latest.release")
}

tasks.named<JavaCompile>("compileJava") {
    options.release.set(17)
    sourceCompatibility = "17"
    targetCompatibility = "17"
}

// Don't actually care about producing an AST jar for the Java in this project, the point is the COBOL ASTs
tasks.named("moderneAst").configure {
    enabled = false;
}
val workaround = tasks.register("createEmptyAstStatsWorkaround") {
    doLast {
        file("build/rewrite/ast-stats.properties").apply{
            parentFile.mkdirs()
            writeText("")
        }
    }
}
tasks.named("moderneMergeProperties").configure {
    dependsOn(workaround)
}

val nistAsts = tasks.register<JavaExec>("nistAsts") {
    val astFile = file("build/rewrite/nist-suite.ast")
    val runtimeClasspath = sourceSets.main.get().runtimeClasspath

    dependsOn(tasks.named("classes"))
    inputs.files(runtimeClasspath)
    inputs.dir(rootProject.file("rewrite-cobol/src/test/resources/gov/nist"))
    outputs.file(astFile)

    args(astFile.toString())
    classpath = runtimeClasspath
    mainClass.set("io.moderne.cobol.CreateNistAstJar")
    javaLauncher.set(javaToolchains.launcherFor {
        languageVersion.set(JavaLanguageVersion.of(17))
    })
}

val nistJar = tasks.register<Jar>("nistJar") {
    from(nistAsts)
    from(tasks.named("moderneMergeProperties"))
    archiveBaseName.set("nist-suite")
    archiveClassifier.set("ast")
}

publishing {
    publications {
        create<MavenPublication>("moderne") {
            artifact(nistJar)
        }
    }
}

artifactory {
    publish {
        setContextUrl("https://artifactory.moderne.ninja/artifactory")
        repository {

            setRepoKey("moderne-public-ast")
            setUsername(System.getenv("AST_PUBLISH_USERNAME"))
            setPassword(System.getenv("AST_PUBLISH_PASSWORD"))
        }
        defaults {
            publications("moderne")
            setProperties(mapOf("moderne_parsed" to "true"))
            setPublishArtifacts(false)
            setPublishPom(false)
        }
    }
    clientConfig.publisher.includePatterns = "*-ast.jar"
    clientConfig.publisher.isFilterExcludedArtifactsFromBuild = true
}
