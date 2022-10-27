plugins {
    id("io.moderne.java-project")
}

dependencies {
    implementation(project(":rewrite-cobol"))
    implementation(project(":rewrite-cobol", "test"))
    implementation("io.github.classgraph:classgraph:latest.release")
}
