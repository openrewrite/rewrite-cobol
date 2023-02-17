plugins {
    id("io.moderne.root-project")
}

nexusPublishing {
    repositories.clear() // root plugin adds nexus snapshots, which we don't want to publish this to
}
