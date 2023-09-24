package sbt.util

import scala.reflect.ClassTag
import scala.annotation.{ meta, StaticAnnotation }
import sjsonnew.{ HashWriter, JsonFormat }
import sjsonnew.support.murmurhash.Hasher
import xsbti.VirtualFile
import java.nio.file.Path

object ActionCache:
  def cache[I: HashWriter, O: JsonFormat: ClassTag](key: I, otherInputs: Long)(
      action: I => (O, Seq[VirtualFile])
  )(
      config: CacheConfiguration
  ): ActionValue[O] =
    val hash: Long = otherInputs * 13L + Hasher.hashUnsafe[I](key)
    val input = ActionInput(hash.toHexString)
    val store = config.store
    val result: Option[ActionValue[O]] = store.get[O](input)
    result match
      case Some(value) =>
        store.syncBlobs(value.outputs, config.outputDirectory)
        value // return the value
      case None =>
        val (newResult, outputs) = action(key)
        val value = store.put[O](input, newResult, outputs)
        store.syncBlobs(value.outputs, config.outputDirectory)
        value
end ActionCache

class CacheConfiguration(
    val store: ActionCacheStore,
    val outputDirectory: Path,
)

@meta.getter
class cacheOptOut(reason: String = "") extends StaticAnnotation
