import sbt._


class Project(info: ProjectInfo) extends AppengineProject(info) with JRebel {
  val snapshots = "snapshots" at "http://scala-tools.org/repo-snapshots"
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.7" % "test"

  //override def testClasspath = super.testClasspath +++ buildCompilerJar

  val sonatypeNexusSnapshots = "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
}

