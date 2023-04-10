rootProject.name = "rewrite-cobol-organizer"

enableFeaturePreview("VERSION_ORDERING_V2")

includeBuild("build-src")

include("rewrite-cobol")
include("publish-nist-asts")
include("publish-nist-asts:src:main:test")
include("rewrite-jcl")
findProject(":publish-nist-asts:src:main:test")?.name = "test"
