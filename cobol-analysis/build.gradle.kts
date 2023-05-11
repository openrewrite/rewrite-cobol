plugins {
    id("io.moderne.java-project")
    id("org.openrewrite.build.language-library") version("latest.release")
}
plugins.apply(org.openrewrite.gradle.RewriteLicensePlugin::class.java)

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

    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:latest.release")
}

tasks.getByName<Test>("test") {
    useJUnitPlatform()
}
