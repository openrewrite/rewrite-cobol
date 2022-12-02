plugins {
    id("io.moderne.java-project")
    id("io.moderne.rewrite")
    id("com.jfrog.artifactory")
}

dependencies {
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
