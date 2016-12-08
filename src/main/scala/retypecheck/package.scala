import scala.reflect.macros.blackbox.Context
import scala.language.implicitConversions

package retypecheck {
  class ReTyperOnContext[+C <: Context](c: C) {
    val retyper: ReTyper[C] = ReTyper(c)
  }
}

package object retypecheck {
  implicit def ReTyperOnContext(c: Context) = new ReTyperOnContext[c.type](c)
}
