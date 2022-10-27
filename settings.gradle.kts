rootProject.name = "rewrite-cobol-organizer"

enableFeaturePreview("VERSION_ORDERING_V2")

includeBuild("build-src")

include("rewrite-cobol")
include("publish-nist-asts")
