pluginManagement {
    repositories {
        mavenLocal()
        gradlePluginPortal()
    }
}

rootProject.name = "rewrite-cobol-organizer"

include("rewrite-cobol")
include("rewrite-jcl")
include("cobol-analysis")
