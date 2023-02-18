plugins {
    id("io.moderne.base")
    id("nebula.release")
    id("io.github.gradle-nexus.publish-plugin")
}

// An interaction with the GCP artifact registry plugin causes a problem with this during release
//nexusPublishing {
//    repositories {
//        sonatype()
//    }
//}

configure<nebula.plugin.release.git.base.ReleasePluginExtension> {
    defaultVersionStrategy = nebula.plugin.release.NetflixOssStrategies.SNAPSHOT(project)
}
