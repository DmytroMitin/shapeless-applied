package applied

import shapeless.SingletonTypeUtils

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox
import scala.language.experimental.macros

class literalFn[A, B](fn: A => B) extends StaticAnnotation

trait Applied[Fn, T] {
  type Out
}

object Applied {
  type Aux[Fn, T, Out0] = Applied[Fn, T] { type Out = Out0 }

  implicit def materialize[Fn, T, Out]: Aux[Fn, T, Out] = macro AppliedMacros.materialize[Fn, T, Out]
}

class AppliedMacros(val c: whitebox.Context) extends SingletonTypeUtils {
  import c.universe._

  def materialize[Fn : WeakTypeTag, T : WeakTypeTag, Out : WeakTypeTag]: Tree = {
    val Fn  = weakTypeOf[Fn].dealias
    val T   = weakTypeOf[T].dealias
    val Out = weakTypeOf[Out].dealias
    println(s"materialize: Fn=$Fn=${showRaw(Fn)}, T=$T=${showRaw(T)}, Out=$Out=${showRaw(Out)}")
    // Out=Out=TypeRef(NoPrefix, TypeName("Out"), List()) not inferred yet
    // have to investigate domain and codomain of function
    val (const, convertToSymbol) = (T, Out) match {
      case (SingletonSymbolType(str), _) =>
        println("materialize: 0")
        Constant(str) -> true
//      case (SingletonSymbolType(str), SingletonSymbolType(_)) =>
//        println("materialize: 1")
//        Constant(str) -> true
//      case (SingletonSymbolType(str), _)                      =>
//        println("materialize: 2")
//        Constant(str) -> false
//      case (ConstantType(cv),         SingletonSymbolType(_)) =>
//        println("materialize: 3")
//        cv            -> true
      case (ConstantType(cv),         _)                      =>
        println("materialize: 4")
        cv            -> false
    }

    // grab the literal function's symbol
    val fnSym = Fn.termSymbol

    // unpack the implementation from its @literalFn annotation
    val (literalFnArg, literalFnImpl) = fnSym.annotations
      .find(_.tree.tpe.typeConstructor <:< weakTypeOf[literalFn[_, _]].typeConstructor)
      .map(_.tree)
      .map(c.untypecheck) // have to undo the typecheck on the tree in order to reuse its bits
      .collect {
        case Apply(_, List(f @ Function(List(arg), body))) => arg -> body
      }
      .getOrElse(c.abort(fnSym.pos, s"$fnSym is not annotated with @literalFn"))

    // construct a tree to evaluate the literal function on the provided type
    val applyTree =
      q"""{
        val ${literalFnArg.name} = $const
        $literalFnImpl
      }"""

    // evaluate it
    val evalExpr = c.Expr[Any](applyTree)

    val result = try {
      c.eval[Any](evalExpr)
    } catch {
      case err: Throwable =>
        val e = err
        println(e)
        throw e
    }

    // put the result back into a constant type
    val outTpe = result match {
      case s: String if convertToSymbol => SingletonSymbolType(s)
      case cv => c.internal.constantType(Constant(cv))
    }

    // construct an instance of Applied
    val anon = TypeName(c.freshName("Applied"))
    val extendsType = appliedType(weakTypeOf[Applied[_, _]].typeConstructor, Fn, T)

    c.typecheck(
      q"""
        final class $anon extends $extendsType {
          type Out = $outTpe
        }
        new $anon: ${appliedType(weakTypeOf[Applied.Aux[_, _, _]], Fn, T, outTpe)}
       """)
  }

}
