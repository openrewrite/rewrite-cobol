plugins {
    id("io.moderne.java-project")
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
