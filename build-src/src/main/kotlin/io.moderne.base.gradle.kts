plugins {
    base
}

group = "io.moderne"

repositories {
    if (!project.hasProperty("releasing")) {
        mavenLocal {
            content {
                excludeVersionByRegex(".+", ".+", ".+-rc[0-9]*")
            }
        }
        maven {
            url = uri("https://oss.sonatype.org/content/repositories/snapshots/")
        }
    }
    mavenCentral {
        content {
            excludeVersionByRegex(".+", ".+", ".+-rc[0-9]*")
        }
    }
}
