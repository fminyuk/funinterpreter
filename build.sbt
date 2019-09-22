name := "fun-expr"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies ++= {
  Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.typelevel" %% "cats-core" % "2.0.0",
    "org.scalatest" %% "scalatest" % "3.0.8" % Test,
    "org.scalamock" %% "scalamock" % "4.4.0" % Test
  )
}
