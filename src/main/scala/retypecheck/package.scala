import scala.language.implicitConversions
import scala.reflect.macros.blackbox

package retypecheck {
  class ReTyperOnContext[+C <: blackbox.Context](c: C) {
    val retyper: ReTyper[C] = ReTyper(c)
  }
}

package object retypecheck {
  implicit def ReTyperOnContext(c: blackbox.Context): ReTyperOnContext[c.type] =
    new ReTyperOnContext(c)
}
