pluginManagement {
    repositories {
        mavenLocal()
        gradlePluginPortal()
    }
}

rootProject.name = "rewrite-cobol-organizer"

includeBuild("build-src")
include("rewrite-cobol")
include("rewrite-jcl")
include("cobol-analysis")
