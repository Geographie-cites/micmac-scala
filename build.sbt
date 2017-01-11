
lazy val monocleVersion = "1.3.2"

lazy val root = Project("micmac", file(".")) settings (
  version := "1.0-SNAPSHOT",
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.0",
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6",
  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
  libraryDependencies += "com.squants"  %% "squants"  % "0.7.1-SNAPSHOT",


  libraryDependencies += "fr.iscpif.freedsl" %% "all" % "0.4",

  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),

  //libraryDependencies += "org.spire-math" %% "kind-projector" % "0.7.1",
  //addCompilerPlugin("org.spire-math" % "kind-projector" % "0.7.1" cross CrossVersion.binary),

  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven",
  resolvers += Resolver.bintrayRepo("projectseptemberinc", "maven"),

  scalacOptions ++= Seq("-Ypartial-unification")
)
