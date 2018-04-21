package sbt

import java.io.File
import java.nio.file.Paths

import sbt.Def.Initialize
import sbt.Def.{ ScopedKey, Setting }
import sbt.Def.ScopedKey
import sbt.Cross._
import sbt.Keys._
import sbt.internal.Act
import sbt.internal.CommandStrings.{
  JavaCrossCommand,
  JavaSwitchCommand,
  javaCrossHelp,
  javaSwitchHelp
}
import scala.collection.breakOut
import sbt.internal.util.complete.DefaultParsers._
import sbt.internal.util.AttributeKey
import sbt.internal.util.complete.{ DefaultParsers, Parser }

object CrossJava {

  private case class JavaHome(home: File, force: Boolean)
  private case class SwitchJavaHome(home: JavaHome, verbose: Boolean, command: Option[String])
  private val JDKVersion = """(\d{1,9})([\.\d{1,9}]?)""".r

  private def switchParser(state: State): Parser[SwitchJavaHome] = {
    import DefaultParsers._
    def versionAndCommand(spacePresent: Boolean) = {
      val x = Project.extract(state)
      import x._
      val javaHomes = getJavaHomes(x, currentRef)
      val knownVersions = javaHomes.keysIterator.toVector
      val version: Parser[JavaHome] = token(StringBasic.examples(knownVersions: _*)).map { arg =>
        val force = arg.endsWith("!")
        val versionArg = if (force) arg.dropRight(1) else arg
        versionArg match {
          case JDKVersion(_, _) =>
            javaHomes.get(versionArg) match {
              case Some(dir) => JavaHome(dir, force)
              case _         => javaHomeNotFound(versionArg)
            }
          case home if new File(home).exists =>
            JavaHome(new File(home), force)
          case _ =>
            sys.error(s"Invalid Java version or home: $versionArg")
        }
      }
      val spacedVersion =
        if (spacePresent) version
        else version & spacedFirst(JavaSwitchCommand)
      val verbose = Parser.opt(token(Space ~> "-v"))
      val optionalCommand = Parser.opt(token(Space ~> matched(state.combinedParser)))
      (spacedVersion ~ verbose ~ optionalCommand).map {
        case v ~ verbose ~ command =>
          SwitchJavaHome(v, verbose.isDefined, command)
      }
    }

    token(JavaSwitchCommand ~> OptSpace) flatMap { sp =>
      versionAndCommand(sp.nonEmpty)
    }
  }

  private def getJavaHomes(extracted: Extracted, proj: ResolvedReference): Map[String, File] = {
    import extracted._
    (fullJavaHomes in proj get structure.data).get
  }

  private def javaHomeNotFound(version: String): Nothing = {
    sys.error(s"""Java home for JDK version $version was not found:
                 |Use Global / unmanagedJavaHomes to set it manually.""".stripMargin)
  }

  private def getCrossJavaHomes(extracted: Extracted, proj: ResolvedReference): Seq[File] = {
    import extracted._
    val fjh = (fullJavaHomes in proj get structure.data).get
    (crossJavaVersions in proj get structure.data) map { jvs =>
      jvs map { jv =>
        fjh.get(jv) match {
          case Some(dir) => dir
          case _         => javaHomeNotFound(jv)
        }
      }
    } getOrElse Vector()
  }

  /**
   * Implements java++ command.
   */
  private def switchCommandImpl(state: State, args: SwitchJavaHome): State = {
    val extracted = Project.extract(state)
    import extracted._

    def switchJavaHome(switch: SwitchJavaHome): State = {
      val projects: Seq[(ResolvedReference, Seq[File])] = {
        structure.allProjectRefs.map(proj => proj -> getCrossJavaHomes(extracted, proj))
      }
      setJavaHomeForProjects(switch.home.home, projects, state, extracted)
    }
    val switchedState = switchJavaHome(args)
    args.command.toList ::: switchedState
  }

  private def setJavaHomeForProjects(
      home: File,
      projects: Seq[(ResolvedReference, Seq[File])],
      state: State,
      extracted: Extracted
  ): State = {
    import extracted._

    val newSettings = projects.flatMap {
      case (project, scalaVersions) =>
        val scope = Scope(Select(project), Zero, Zero, Zero)

        Seq(
          javaHome in scope := Some(home)
        )
    }

    val filterKeys: Set[AttributeKey[_]] = Set(javaHome).map(_.key)

    val projectsContains: Reference => Boolean = projects.map(_._1).toSet.contains

    // Filter out any old scala version settings that were added, this is just for hygiene.
    val filteredRawAppend = session.rawAppend.filter(_.key match {
      case ScopedKey(Scope(Select(ref), Zero, Zero, Zero), key)
          if filterKeys.contains(key) && projectsContains(ref) =>
        false
      case _ => true
    })

    val newSession = session.copy(rawAppend = filteredRawAppend ++ newSettings)

    BuiltinCommands.reapply(newSession, structure, state)
  }

  def switchJavaHome: Command =
    Command.arb(requireSession(switchParser), javaSwitchHelp)(switchCommandImpl)

  def crossJavaHome: Command =
    Command.arb(requireSession(crossParser), javaCrossHelp)(crossJavaHomeCommandImpl)

  private case class CrossArgs(command: String, verbose: Boolean)

  /**
   * Parse the given command into either an aggregate command or a command for a project
   */
  private def crossParser(state: State): Parser[CrossArgs] =
    token(JavaCrossCommand <~ OptSpace) flatMap { _ =>
      (token(Parser.opt("-v" <~ Space)) ~ token(matched(state.combinedParser))).map {
        case (verbose, command) => CrossArgs(command, verbose.isDefined)
      } & spacedFirst(JavaCrossCommand)
    }

  private def crossJavaHomeCommandImpl(state: State, args: CrossArgs): State = {
    val x = Project.extract(state)
    import x._

    println(s"parsed out $args")

    val (aggs, aggCommand) = Cross.parseSlashCommand(x)(args.command)
    val projCrossVersions = aggs map { proj =>
      proj -> getCrossJavaHomes(x, proj)
    }
    // if we support javaHome, projVersions should be cached somewhere since
    // running ++2.11.1 is at the root level is going to mess with the scalaVersion for the aggregated subproj
    val projVersions = (projCrossVersions flatMap {
      case (proj, versions) => versions map { proj.project -> _ }
    }).toList

    val verbose = ""

    println(s"projVersions $projVersions")

    if (projVersions.isEmpty) {
      state
    } else {
      // Detect whether a task or command has been issued
      val allCommands = Parser.parse(aggCommand, Act.aggregatedKeyParser(x)) match {
        case Left(_) =>
          // It's definitely not a task, check if it's a valid command, because we don't want to emit the warning
          // message below for typos.
          val validCommand = Parser.parse(aggCommand, state.combinedParser).isRight

          val distinctCrossConfigs = projCrossVersions.map(_._2.toSet).distinct
          if (validCommand && distinctCrossConfigs.size > 1) {
            state.log.warn(
              "Issuing a Java cross building command, but not all sub projects have the same cross build " +
                "configuration. This could result in subprojects cross building against Java versions that they are " +
                "not compatible with. Try issuing cross building command with tasks instead, since sbt will be able " +
                "to ensure that cross building is only done using configured project and Java version combinations " +
                "that are configured.")
            state.log.debug("Java versions configuration is:")
            projCrossVersions.foreach {
              case (project, versions) => state.log.debug(s"$project: $versions")
            }
          }

          // Execute using a blanket switch
          projCrossVersions.toMap.apply(currentRef).flatMap { version =>
            // Force scala version
            Seq(s"$JavaSwitchCommand $verbose $version!", aggCommand)
          }

        case Right(_) =>
          // We have a key, we're likely to be able to cross build this using the per project behaviour.

          // Group all the projects by scala version
          projVersions.groupBy(_._2).mapValues(_.map(_._1)).toSeq.flatMap {
            case (version, Seq(project)) =>
              // If only one project for a version, issue it directly
              Seq(s"$JavaSwitchCommand $verbose $version", s"$project/$aggCommand")
            case (version, projects) if aggCommand.contains(" ") =>
              // If the command contains a space, then the `all` command won't work because it doesn't support issuing
              // commands with spaces, so revert to running the command on each project one at a time
              s"$JavaSwitchCommand $verbose $version" :: projects.map(project =>
                s"$project/$aggCommand")
            case (version, projects) =>
              // First switch scala version, then use the all command to run the command on each project concurrently
              Seq(s"$JavaSwitchCommand $verbose $version",
                  projects.map(_ + "/" + aggCommand).mkString("all ", " ", ""))
          }
      }

      allCommands.toList ::: captureCurrentSession(state, x)
    }
  }

  private val CapturedSession = AttributeKey[Seq[Setting[_]]]("crossCapturedSession")

  private def captureCurrentSession(state: State, extracted: Extracted): State = {
    state.put(CapturedSession, extracted.session.rawAppend)
  }

  final val jvmDirectories = Seq(
    ("/usr/lib/jvm", "java-([0-9]+)-.*".r, ""),
    ("/Library/Java/JavaVirtualMachines", "jdk[1\\.]*([0-9]+)\\..*".r, "Contents/Home")
  )

  private[sbt] def discoverJavaHomes(): Initialize[Map[String, File]] = Def.setting {
    jvmDirectories
      .flatMap {
        case (root, dirRegexp, inner) =>
          Option(new File(root).list())
            .getOrElse(Array.empty[String])
            .toSeq
            .collect {
              case dir @ dirRegexp(ver) => ver -> Paths.get(root, dir, inner).toFile
            }
      }(breakOut)
  }

}
