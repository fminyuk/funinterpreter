name := "fun-expr"

version := "0.1"

scalaVersion := "2.12.0"

libraryDependencies ++= {
  Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "org.typelevel" %% "cats-core" % "2.0.0-RC1",
    "org.scalatest" %% "scalatest" % "3.0.8" % "test"
  )
}
