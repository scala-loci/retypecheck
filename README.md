# retypecheck

The Scala type-checking process distorts certain ASTs (such as representations
of extractors or case classes) in a way that they cannot be type-checked again.
The issue is described in [SI-5464](http://issues.scala-lang.org/browse/SI-5464).
Ideally, it should be possible to 1) type-check an AST, 2) transform the AST and
3) re-type-check the transformed AST. This does not work for all ASTs using the
`typecheck` and `untypecheck` methods provided by the Scala reflection library.

On a `Context` `c`, *retypecheck* provides the methods `c.retyper.typecheck`,
`c.retyper.untypecheck` (based on standard `c.untypecheck`),
`c.retyper.untypecheckAll` (based on `c.resetAllAttrs` of the
*[resetallattrs](http://github.com/scalamacros/resetallattrs)* library),
`c.retyper.retypecheck` (using `retyper.untypecheck` followed by
`retyper.typecheck`), `c.retyper.retypecheckAll` (using `retyper.untypecheckAll`
followed by `retyper.typecheck`). These wrapper methods around the
type-checking/un-type-checking process try to fix up the AST in a way that it
can be type-checked again on a best effort basis. They do not aim at undoing all
of the compiler's desugerings during type-checking.

## How-To

1. Add the resolver to your `build.sbt`:

   ```scala
   resolvers += Resolver.bintrayRepo("stg-tud", "maven")
   ```
   
2. Add the dependency to your `build.sbt`:

   ```scala
   libraryDependencies += "de.tuda.stg" %% "retypecheck" % "0.5.0"
   ```

### Example Usage

```scala
import retypecheck._

def impl(c: Context)(...) = {
  ...
  c.retyper.typecheck(...)
  c.retyper.retypecheck(...)
  c.retyper.untypecheck(...)
  ...
}
```
