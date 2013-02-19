scalaVersion := "2.9.1"

libraryDependencies  ++=  Seq(
  "org.squeryl" %% "squeryl" % "0.9.5-RC1",
  "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
  "com.itextpdf" % "itextpdf"  % "5.3.2",
  "org.apache.xmlgraphics" % "batik-dom" % "1.7",
  "org.apache.xmlgraphics" % "batik-bridge" % "1.7",
  "org.apache.xmlgraphics" % "batik-svggen" % "1.7"
)

libraryDependencies += "net.liftweb" %% "lift-json" % "2.4-M5"

