plugins {
    id("org.openrewrite.build.language-library")
}

group = "org.openrewrite.recipe"
description = "OpenRewrite recipes for analyzing COBOL sources"

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
    implementation(project(":rewrite-cobol"))
    implementation(project(":rewrite-jcl"))
    implementation("org.openrewrite:rewrite-core")

    testImplementation(project(":rewrite-cobol"))
    testImplementation(project(":rewrite-cobol", "test"))
    testImplementation(project(":rewrite-jcl"))
    testImplementation("org.openrewrite:rewrite-test")
    testImplementation("org.junit-pioneer:junit-pioneer:2.0.0")
    testImplementation("org.junit.jupiter:junit-jupiter-api:latest.release")
    testImplementation("org.junit.jupiter:junit-jupiter-params:latest.release")
    testImplementation("io.github.classgraph:classgraph:latest.release")

    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:latest.release")
}
