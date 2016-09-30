import scala.annotation._
import scala.meta._
import scala.meta.transversers._

class memoized extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    def cacheName(methodName: Term.Name) = // trees can be constructed directly
      Pat.Var.Term(Term.Name(s"cache$$${methodName.toString}")) // unique, yeah?

    val q"..$mods object $name extends $template" = defn

    // cannot be generated during "transform" stage, thanks to Block-ing of several expressions
    val caches = template.collect {
      // not most general pattern for defs
      case q"..$mods def $methodName[..$tparams]($value: $tpe): $tpeopt = { ..$stats }" =>
        // for convenience
        val keyType = tpe.get.asInstanceOf[Type]
        val valueType = tpeopt.get.asInstanceOf[Type]
        val name = cacheName(methodName)

        q"val $name = new scala.collection.mutable.HashMap[$keyType, $valueType]"
    }

    val newTemplate = template.transform {
      case q"..$mods def $methodName[..$tparams]($value: $tpe): $tpeopt = { ..$stats }" =>
        q"""
            ..$mods def $methodName[..$tparams]($value: $tpe): $tpeopt = {
              ${cacheName(methodName).name}.getOrElseUpdate(${value.asInstanceOf[Term.Arg]}, {..$stats})
            }
          """
    } match {
      case template"{ ..$stats1 } with ..$ctorcalls { $param => ..$stats2 }" =>
        template"""
        { ..$stats1 } with ..$ctorcalls { $param =>
          ..${caches ++ stats2}
        }
          """
    }

    val result = q"..$mods object $name extends $newTemplate"
    println(result.show[Syntax] + "\n\n---------------------------------------\n")
    result
  }
}