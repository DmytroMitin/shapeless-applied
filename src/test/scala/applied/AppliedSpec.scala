package applied

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import shapeless.Witness

class AppliedSpec extends AnyFreeSpec with Matchers {

  "Symbols" - {
    "addFoo" in {
      def result[In <: Symbol, Out <: Symbol](in: Witness.Lt[In])(implicit
        applied: Applied.Aux[addFoo.type, In, Out],
        witnessResult: Witness.Aux[Out]
      ): Symbol = witnessResult.value

      result('blah) shouldEqual 'blahFoo
    }

    "camelCase" in {
      def result[In <: Symbol, Out <: Symbol](in: Witness.Lt[In])(implicit
        applied: Applied.Aux[camelCase.type, In, Out],
        witnessResult: Witness.Aux[Out]
      ): Symbol = witnessResult.value

      result('smorgas_bord) shouldEqual 'SmorgasBord
    }
  }

  "Strings" - {
    "addFoo" in {
      def result[In <: String, Out <: String](in: Witness.Lt[In])(implicit
        applied: Applied.Aux[addFoo.type, In, Out],
        witnessResult: Witness.Aux[Out]
      ): String = witnessResult.value

      result("blah") shouldEqual "blahFoo"
    }

    "camelCase" in {
      def result[In <: String, Out <: String](in: Witness.Lt[In])(implicit
        applied: Applied.Aux[camelCase.type, In, Out],
        witnessResult: Witness.Aux[Out]
      ): String = witnessResult.value

      result("smorgas_bord") shouldEqual "SmorgasBord"
    }
  }


  "Ints" in {
    def result[In <: Int, Out <: Int](in: Witness.Lt[In])(implicit
      applied: Applied.Aux[numDigits.type, In, Out],
      witnessResult: Witness.Aux[Out]
    ): Int = witnessResult.value

    result(123) shouldEqual 3
  }

  "Booleans" in {
    def result[In <: Boolean, Out <: Boolean](in: Witness.Lt[In])(implicit
      applied: Applied.Aux[notb.type, In, Out],
      witnessResult: Witness.Aux[Out]
    ): Boolean = witnessResult.value

    result(true) shouldEqual false
    result(false) shouldEqual true
  }

  "Floats" in {
    def result[In <: Float, Out <: Float](in: Witness.Lt[In])(implicit
      applied: Applied.Aux[timesTwoPointTwo.type, In, Out],
      witnessResult: Witness.Aux[Out]
    ): Float = witnessResult.value

    result(10.10f) shouldEqual (10.10f * 2.2f)
  }

  "Doubles" in {
    def result[In <: Double, Out <: Double](in: Witness.Lt[In])(implicit
      applied: Applied.Aux[exp.type, In, Out],
      witnessResult: Witness.Aux[Out]
    ): Double = witnessResult.value

    result(10.10) shouldEqual math.exp(10.10)
  }

  "Ints to Strings" in {
    def result[In <: Int, Out <: String](in: Witness.Lt[In])(implicit
      applied: Applied.Aux[intToStr.type, In, Out],
      witnessResult: Witness.Aux[Out]
    ): String = witnessResult.value

    result(123) shouldEqual "123"
  }

  "Strings to Symbols" in {
    def result[In <: String, Out <: Symbol](in: Witness.Lt[In])(implicit
      applied: Applied.Aux[strToSymb.type, In, Out],
      witnessResult: Witness.Aux[Out]
    ): Symbol = witnessResult.value

//    result("blah") shouldEqual 'blah
    //Information: applied.this.Applied.materialize is not a valid implicit value for applied.Applied.Aux[applied.strToSymb.type,this.T,Out] because:
    //hasMatchingSymbol reported error: exception during macro expansion:
    //java.lang.Error: bad constant value: Symbol(blah) of class class scala.Symbol
    //	at scala.reflect.internal.Constants$Constant.<init>(Constants.scala:58)
    //	at scala.reflect.internal.Constants$Constant$.apply(Constants.scala:40)
    //	at scala.reflect.internal.Constants$Constant$.apply(Constants.scala:292)
    //	at applied.AppliedMacros.materialize(Applied.scala:84)
  }

  "Symbols to Strings" in {
    def result[In <: Symbol, Out <: String](in: Witness.Lt[In])(implicit
      applied: Applied.Aux[symbToStr.type, In, Out],
      witnessResult: Witness.Aux[Out]
    ): String = witnessResult.value

//    result('blah) shouldEqual "blah"
    //Information: applied.this.Applied.materialize is not a valid implicit value for applied.Applied.Aux[applied.symbToStr.type,this.T,Out] because:
    //hasMatchingSymbol reported error: exception during macro expansion:
    //java.lang.ClassCastException: java.lang.String cannot be cast to scala.Symbol
    //	at __wrapper$1$4c4a668bfdcb4c0889fa3956a5de81fc.__wrapper$1$4c4a668bfdcb4c0889fa3956a5de81fc$.wrapper(<no source file>:156)
    //	at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
    //	at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
    //	at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
    //	at java.lang.reflect.Method.invoke(Method.java:498)
    //	at scala.tools.reflect.ToolBoxFactory$ToolBoxImpl$ToolBoxGlobal.$anonfun$compile$11(ToolBoxFactory.scala:291)
    //	at scala.tools.reflect.ToolBoxFactory$ToolBoxImpl.eval(ToolBoxFactory.scala:460)
    //	at scala.reflect.macros.contexts.Evals.eval(Evals.scala:32)
    //	at scala.reflect.macros.contexts.Evals.eval$(Evals.scala:26)
    //	at scala.reflect.macros.contexts.Context.eval(Context.scala:18)
    //	at applied.AppliedMacros.materialize(Applied.scala:73)
  }

}

@literalFn((s: String) => s + "Foo")
object addFoo extends (String => String) {
  def apply(s: String): String = s + "Foo"
}

@literalFn((s: String) => """(?:^|_)([a-z])""".r.replaceAllIn(s, m => m.group(1).toUpperCase))
object camelCase extends (String => String) {
  def apply(s: String): String = """(?:^|_)([a-z])""".r.replaceAllIn(s, m => m.group(1).toUpperCase)
}

@literalFn((i: Int) => i.toString.length)
object numDigits extends (Int => Int) {
  def apply(i: Int): Int = i.toString.length
}

@literalFn((b: Boolean) => !b)
object notb extends (Boolean => Boolean) {
  def apply(b: Boolean): Boolean = !b
}

@literalFn((f: Float) => f * 2.2f)
object timesTwoPointTwo extends (Float => Float) {
  def apply(f: Float): Float = f * 2.2f
}

@literalFn((d: Double) => math.exp(d))
object exp extends (Double => Double) {
  def apply(d: Double): Double = math.exp(d)
}

@literalFn((i: Int) => i.toString)
object intToStr extends (Int => String) {
  def apply(i: Int): String = i.toString
}

@literalFn((s: String) => Symbol(s))
object strToSymb extends (String => Symbol) {
  def apply(s: String): Symbol = Symbol(s)
}

@literalFn((s: Symbol) => s.name)
object symbToStr extends (Symbol => String) {
  def apply(s: Symbol): String = s.name
}