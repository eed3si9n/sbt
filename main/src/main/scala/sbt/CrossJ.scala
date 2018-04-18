package sbt

import java.io.File

import sbt.Def.{ ScopedKey, Setting }
import sbt.Def.ScopedKey
import sbt.Cross.{ crossVersions, switchScalaVersion, _ }
import sbt.Keys._
import sbt.internal.Act
import sbt.internal.CommandStrings.{
  JavaHomeCrossCommand,
  JavaHomeSwitchCommand,
  crossHelp,
  switchHelp
}
import sbt.internal.util.complete.DefaultParsers._
import sbt.internal.util.AttributeKey
import sbt.internal.util.complete.{ DefaultParsers, Parser }

object CrossJ {

  private case class JavaHome(home: File)
  private case class SwitchJavaHome(home: JavaHome)

  private def switchParser(state: State): Parser[SwitchJavaHome] = {
    import DefaultParsers._
    def versionAndCommand(spacePresent: Boolean) = {
      val x = Project.extract(state)
      import x._
      val knownVersions = crossJavaHome(x, currentRef).map(_.getAbsolutePath)
      val version = token(StringBasic.examples(knownVersions: _*)).map { arg =>
        arg
      }
      val spacedVersion =
        if (spacePresent) version else version & spacedFirst(JavaHomeSwitchCommand)

      spacedVersion.map(v => SwitchJavaHome(JavaHome(new File(v))))
    }

    token(JavaHomeSwitchCommand ~> OptSpace) flatMap { sp =>
      versionAndCommand(sp.nonEmpty)
    }
  }

  private def crossJavaHome(extracted: Extracted, proj: ResolvedReference): Seq[File] = {
    import extracted._
    (crossJavaHomes in proj get structure.data) getOrElse {
      // reading scalaVersion is a one-time deal
      (javaHome in proj get structure.data).toSeq
    }.flatten
  }

  private def switchCommandImpl(state: State, args: SwitchJavaHome): State = {
    switchJavaHome(args, state)
  }

  private def switchJavaHome(switch: SwitchJavaHome, state: State): State = {
    val extracted = Project.extract(state)
    import extracted._

    val projects: Seq[(ResolvedReference, Seq[File])] = {
      structure.allProjectRefs.map(proj => proj -> crossJavaHome(extracted, proj))
    }

    setScalaVersionForProjects(switch.home.home, projects, state, extracted)
  }

  private def setScalaVersionForProjects(
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
    Command.arb(requireSession(switchParser), switchHelp)(switchCommandImpl)

  def crossJavaHome: Command =
    Command.arb(requireSession(crossParser), crossHelp)(crossJavaHomeImpl)

  private case class CrossArgs(command: String, verbose: Boolean)

  /**
   * Parse the given command into either an aggregate command or a command for a project
   */
  private def crossParser(state: State): Parser[CrossArgs] =
    token(JavaHomeCrossCommand <~ OptSpace) flatMap { _ =>
      (token(Parser.opt("-v" <~ Space)) ~ token(matched(state.combinedParser))).map {
        case (verbose, command) => CrossArgs(command, verbose.isDefined)
      } & spacedFirst(JavaHomeCrossCommand)
    }

  private def resolveAggregates(extracted: Extracted): Seq[ProjectRef] = {
    import extracted._

    def findAggregates(project: ProjectRef): List[ProjectRef] = {
      project :: (structure.allProjects(project.build).find(_.id == project.project) match {
        case Some(resolved) => resolved.aggregate.toList.flatMap(findAggregates)
        case None           => Nil
      })
    }

    (currentRef :: currentProject.aggregate.toList.flatMap(findAggregates)).distinct
  }

  private def parseCommand(command: String): Either[String, (String, String)] = {
    import DefaultParsers._
    val parser = (OpOrID <~ charClass(_ == '/', "/")) ~ any.* map {
      case project ~ cmd => (project, cmd.mkString)
    }
    Parser.parse(command, parser).left.map(_ => command)
  }

  private def crossJavaHomeImpl(state: State, args: CrossArgs): State = {
    val x = Project.extract(state)
    import x._

    println(s"parsed out $args")

    val (aggs, aggCommand) = parseCommand(args.command) match {
      case Right((project, cmd)) =>
        (structure.allProjectRefs.filter(_.project == project), cmd)
      case Left(cmd) => (resolveAggregates(x), cmd)
    }

    println(s"(aggs, aggCommand) ($aggs, $aggCommand)")

    val projCrossVersions = aggs map { proj =>
      proj -> crossJavaHome(x, proj)
    }
    // if we support scalaVersion, projVersions should be cached somewhere since
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
              "Issuing a cross building command, but not all sub projects have the same cross build " +
                "configuration. This could result in subprojects cross building against Scala versions that they are " +
                "not compatible with. Try issuing cross building command with tasks instead, since sbt will be able " +
                "to ensure that cross building is only done using configured project and Scala version combinations " +
                "that are configured.")
            state.log.debug("Scala versions configuration is:")
            projCrossVersions.foreach {
              case (project, versions) => state.log.debug(s"$project: $versions")
            }
          }

          // Execute using a blanket switch
          projCrossVersions.toMap.apply(currentRef).flatMap { version =>
            // Force scala version
            Seq(s"$JavaHomeSwitchCommand $verbose $version!", aggCommand)
          }

        case Right(_) =>
          // We have a key, we're likely to be able to cross build this using the per project behaviour.

          // Group all the projects by scala version
          projVersions.groupBy(_._2).mapValues(_.map(_._1)).toSeq.flatMap {
            case (version, Seq(project)) =>
              // If only one project for a version, issue it directly
              Seq(s"$JavaHomeSwitchCommand $verbose $version", s"$project/$aggCommand")
            case (version, projects) if aggCommand.contains(" ") =>
              // If the command contains a space, then the `all` command won't work because it doesn't support issuing
              // commands with spaces, so revert to running the command on each project one at a time
              s"$JavaHomeSwitchCommand $verbose $version" :: projects.map(project =>
                s"$project/$aggCommand")
            case (version, projects) =>
              // First switch scala version, then use the all command to run the command on each project concurrently
              Seq(s"$JavaHomeSwitchCommand $verbose $version",
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

}
