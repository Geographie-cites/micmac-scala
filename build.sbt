
lazy val monocleVersion = "1.2.2"

lazy val root = Project("micmac", file(".")) settings (
  version := "1.0-SNAPSHOT",
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.11.8",
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6",
  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
  libraryDependencies +=  "com.github.julien-truffaut"  %%  "monocle-state"   % monocleVersion,

  libraryDependencies += "com.projectseptember" %% "freek" % "0.6.0",
  libraryDependencies += "org.spire-math" %% "kind-projector" % "0.7.1",

  libraryDependencies += "fr.iscpif.freedsl" %% "all" % "1.0-SNAPSHOT",

  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.7.1" cross CrossVersion.binary),

  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven",
  resolvers += Resolver.bintrayRepo("projectseptemberinc", "maven"),

  scalacOptions ++= Seq("-Ypartial-unification")
)
