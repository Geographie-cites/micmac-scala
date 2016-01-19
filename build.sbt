
version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers += "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven"

libraryDependencies += "com.codecommit" %% "emm" % "0.1-c65281a"

resolvers += Resolver.sonatypeRepo("snapshots") 

