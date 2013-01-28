import sbt._
import sbt.Keys._

import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi.osgiSettings


/**
 * The '''Smocs''' `object` defines the
 * [[http://www.scala-sbt.org/release/docs sbt]] build settings for the
 * Scala Monadic Constraint Solver project.
 *
 * Inspiration for the structure was drawn from the AkkaBuild.scala code
 * within akka/project.  Many thanks go to the people involved in that!
 *
 * @author	svickers
 *
 */
object Smocs
	extends Build
{
	/// Class Types
	object OSGi
	{
		/// Instance Properties
		val defaultImports = Seq (
			"!sun.misc",
			"*"
			);

		val api = exports ("com.tubros.constraints.api.*") ++ imports ();

		val core = exports ("com.tubros.constraints.core.*") ++ imports ();


		def exports (packages : String *) = osgiSettings ++ Seq (
			OsgiKeys.exportPackage := packages
			);

		def imports (packages : String *) = Seq (
			OsgiKeys.importPackage := packages ++ defaultImports
			);
	}


	/// Instance Properties
	lazy val buildSettings = Seq (
		organization := "com.tubros",
		version := "0.1-SNAPSHOT",
		scalaVersion := "2.10.0"
		);

	/// sbt settings applicable to any build
	lazy val defaultSettings = Defaults.defaultSettings ++ Seq (
		scalacOptions in Compile ++= Seq (
			"-encoding", "UTF-8",
			"-target:jvm-1.6",
			"-deprecation",
			"-feature",
			"-unchecked"
			)
		);

	override lazy val settings = super.settings ++ buildSettings;

	lazy val root = Project (
		id = "smocs",
		base = file ("."),
		aggregate = Seq (api, core),
		settings = defaultSettings ++ Seq (
			libraryDependencies ++= Dependencies.smocs
			)
		);

	lazy val api = Project (
		id = "smocs-api",
		base = file ("smocs-api"),
		settings = defaultSettings ++ OSGi.api ++ Seq (
			publishArtifact in (Test, packageBin) := true,
			libraryDependencies ++= Dependencies.api
			)
		);

	lazy val core = Project (
		id = "smocs-core",
		base = file ("smocs-core"),
		dependencies = Seq (api % "compile->compile;test->test"),
		settings = defaultSettings ++ OSGi.core ++ Seq (
			libraryDependencies ++= Dependencies.core
			)
		);


		/// Constructor Body
}


object Dependencies
{
	object Graph
	{
		private val version = "1.6.0";

		lazy val core = Seq (
			"com.assembla.scala-incubator" % "graph-core_2.10" % version,
			"com.assembla.scala-incubator" % "graph-constrained_2.10" % version
			);
	}


	object Scalaz
	{
		private val version = "7.0.0-M7";

		lazy val core = Seq (
			"org.scalaz" % "scalaz-core_2.10" % version,
			"org.scalaz" % "scalaz-typelevel_2.10" % version
			);

		lazy val effect = Seq (
			"org.scalaz" % "scalaz-effect_2.10" % version
			);

		lazy val iteratee = Seq (
			"org.scalaz" % "scalaz-iteratee_2.10" % version
			);

		lazy val xml = Seq (
			"org.scalaz" % "scalaz-xml_2.10" % version
			);

		lazy val all = core ++ effect ++ iteratee ++ xml;
	}


	object OSGi
	{
		private val version = "4.3.1";

		val core = Seq (
			"org.osgi" % "org.osgi.core" % version % "provided"
			);

		val compendium = Seq (
			"org.osgi" % "org.osgi.compendium" % version % "provided"
			);

		val all = compendium ++ core;
	}


	/// Compile Dependencies
	lazy val compile = Scalaz.core ++ OSGi.all ++ Seq (
		"org.scala-lang" % "scala-reflect" % "2.10.0"
		);

	/// Test Dependencies
	lazy val test = Seq (
		"junit" % "junit" % "4.10" % "test",
		"org.hamcrest" % "hamcrest-core" % "1.3" % "test",
		"org.hamcrest" % "hamcrest-library" % "1.3" % "test",
		"org.jmock" % "jmock" % "2.5.1" % "test",
		"org.jmock" % "jmock-junit4" % "2.5.1" % "test" intransitive(),
		"org.jmock" % "jmock-legacy" % "2.5.1" % "test",
		"org.scalacheck" % "scalacheck_2.10" % "1.10.0" % "test",
		"org.scalamock" % "scalamock-core_2.10" % "3.0" % "test",
		"org.scalamock" % "scalamock-scalatest-support_2.10" % "3.0" % "test",
		"org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test",
		"org.slf4j" % "slf4j-jdk14" % "1.7.2" % "test"
		);

	lazy val smocs = compile;
	lazy val api = compile ++ test;
	lazy val core = compile ++ Graph.core ++ test;
}

