/* sbt -- Simple Build Tool
 * Copyright 2008, 2009, 2010, 2011 Mark Harrah
 */
package xsbt

import java.io.File
import java.util.{ Arrays, Comparator }
import scala.tools.nsc.{ io, plugins, symtab, Global, Phase }
import io.{ AbstractFile, PlainFile, ZipArchive }
import plugins.{ Plugin, PluginComponent }
import symtab.Flags
import scala.collection.mutable.{ HashMap, HashSet, ListBuffer }
import xsbti.api._

object API {
  val name = "xsbt-api"
}

final class API(val global: CallbackGlobal) extends Compat {
  import global._

  @inline def debug(msg: => String) = if (settings.verbose.value) inform(msg)

  def newPhase(prev: Phase) = new ApiPhase(prev)
  class ApiPhase(prev: Phase) extends GlobalPhase(prev) {
    override def description = "Extracts the public API from source files."
    def name = API.name
    override def run(): Unit =
      {
        val start = System.currentTimeMillis
        super.run
        val stop = System.currentTimeMillis
        debug("API phase took : " + ((stop - start) / 1000.0) + " s")
      }

    def apply(unit: global.CompilationUnit): Unit = processUnit(unit)

    def processUnit(unit: CompilationUnit) = if (!unit.isJava) processScalaUnit(unit)
    def processScalaUnit(unit: CompilationUnit): Unit = {
      val sourceFile = unit.source.file.file
      debug("Traversing " + sourceFile)
      callback.startSource(sourceFile)
      val extractApi = new ExtractAPI[global.type](global, sourceFile)
      val traverser = new TopLevelHandler(extractApi)
      traverser.apply(unit.body)
      if (global.callback.nameHashing) {
        val extractUsedNames = new ExtractUsedNames[global.type](global)
        val names = extractUsedNames.extract(unit)
        debug("The " + sourceFile + " contains the following used names " + names)
        names foreach { (name: String) => callback.usedName(sourceFile, name) }
        val extractDeclaredClasses = new ExtractDeclaredClasses[global.type](global)
        val declaredClasses = extractDeclaredClasses.extract(unit)
        debug("The " + sourceFile + " contains the following declared classes " + declaredClasses)
        declaredClasses foreach { (declaredClass: String) => callback.declaredClass(sourceFile, declaredClass) }
      }
      extractApi.forceStructures()
      val classApis = traverser.allNonLocalClasses

      classApis.foreach(callback.api(sourceFile, _))
    }
  }

  private final class TopLevelHandler(extractApi: ExtractAPI[global.type]) extends TopLevelTraverser {
    def allNonLocalClasses: Set[ClassLike] = {
      extractApi.forceStructures()
      extractApi.allExtractedNonLocalClasses
    }
    def `class`(c: Symbol): Unit = {
      extractApi.extractAllClassesOf(c.owner, c)
    }
  }

  private abstract class TopLevelTraverser extends Traverser {
    def `class`(s: Symbol)
    override def traverse(tree: Tree): Unit = {
      tree match {
        case (_: ClassDef | _: ModuleDef) if isTopLevel(tree.symbol) => `class`(tree.symbol)
        case _: PackageDef =>
          super.traverse(tree)
        case _ =>
      }
    }
    def isTopLevel(sym: Symbol): Boolean =
      (sym ne null) && (sym != NoSymbol) && !sym.isImplClass && !sym.isNestedClass && sym.isStatic &&
        !sym.hasFlag(Flags.SYNTHETIC) && !sym.hasFlag(Flags.JAVA)
  }

}
