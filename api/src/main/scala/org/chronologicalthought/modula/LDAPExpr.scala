/*
 * Copyright 2013 David Savage
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.chronologicalthought.modula

import LDAPExpr._
import util.matching.Regex
import annotation.tailrec
import java.{util => ju}
import collection.JavaConversions._
import scala.util.parsing.combinator.RegexParsers
import java.lang.{IllegalArgumentException, UnsupportedOperationException, IllegalStateException}

/**
 * @author David Savage
 */

object LDAPExpr {
  def apply(str: String): LDAPExpr = {
    if (str == null || str.trim.length == 0)
      throw new IllegalArgumentException("""Invalid ldap expr """" + str)
    else {
      val p = new LDAPParser
      p.parseAll(p.expr, str) match {
        case s: p.Success[LDAPExpr] => s.get
        case f: p.NoSuccess => throw new IllegalArgumentException("Invalid ldap expr " + str + " -> " + f.msg)
      }
    }
  }

  def expr(name: String, op: Op, value: String) = {
    new Simple(name, op, value)
  }

  def and(left: LDAPExpr, right: LDAPExpr) = new And(left :: right :: Nil).simplify

  def or(left: LDAPExpr, right: LDAPExpr) = new Or(left :: right :: Nil).simplify

  def not(expr: LDAPExpr) = new Not(expr)

  private val WildCard = "*"

  sealed abstract class Op extends ((Any, String) => Boolean) {
    def apply(left: Any, right: String): Boolean = {
      left match {
        case c: ju.Collection[Any] => apply(c.toList, right)
        case a: Array[_] => apply(a.toList, right)
        case head :: rest => if (apply(head, right)) true else apply(rest, right)
        case Nil => false
        case any => op(left, right)
      }
    }

    def op(left: Any, right: String): Boolean
  }

  val `=` = new EqualOp
  val `~=` = new ApproxEqualOp
  val `>` = new GreaterThanOp
  val `>=` = new GreaterThanOrEqualOp
  val `<` = new LessThanOp
  val `<=` = new LessThanOrEqualOp

  case class EqualOp extends Op {
    def op(left: Any, right: String) = right == WildCard || {
      if (right contains WildCard) {
        val pattern = new Regex(right.replace(WildCard, ".*")).pattern
        pattern.matcher(left.toString).matches
      }
      else {
        val l = new AnyOrdered(left)
        val r = new AnyOrdered(right)
        l == r
      }
    }

    override def toString = "="
  }

  // TODO implement approx equals properly
  // The evaluation of the approximate match (’~=’) filter type is
  // implementation specific but should at least ignore case and
  // white space differences. Codes such as soundex or other smart
  // closeness comparisons may be used.
  case class ApproxEqualOp extends Op {
    def op(left: Any, right: String) = {
      val rightUpper = right.toUpperCase.replace(" ", "")
      if (rightUpper contains WildCard) {
        val pattern = new Regex(rightUpper.replace(WildCard, ".*")).pattern
        pattern.matcher(left.toString.toUpperCase).matches
      }
      else {
        val l = new AnyOrdered(left, (in: String) => in.toUpperCase)
        val r = new AnyOrdered(rightUpper)
        l == r
      }
    }

    override def toString = "~="
  }

  case class GreaterThanOp extends Op {
    def op(left: Any, right: String) = {
      val l = new AnyOrdered(left)
      val r = new AnyOrdered(right)
      l > r
    }

    override def toString = ">"
  }

  case class LessThanOp extends Op {
    def op(left: Any, right: String) = {
      val l = new AnyOrdered(left)
      val r = new AnyOrdered(right)
      l < r
    }

    override def toString = "<"
  }

  case class GreaterThanOrEqualOp extends Op {
    def op(left: Any, right: String) = {
      val l = new AnyOrdered(left)
      val r = new AnyOrdered(right)
      l >= r
    }

    override def toString = ">="
  }

  case class LessThanOrEqualOp extends Op {
    def op(left: Any, right: String) = {
      val l = new AnyOrdered(left)
      val r = new AnyOrdered(right)
      l <= r
    }

    override def toString = "<="
  }

  case class Simple(name: String, op: Op, value: String) extends LDAPExpr {
    def apply(map: Map[String, Any]) = map.get(name) match {
      case Some(v) => op(v, value)
      case None => false
    }

    override def toString = name + op + value
  }

  val True = new TrueExpr
  val False = new FalseExpr

  case class TrueExpr extends LDAPExpr {
    override def apply(map: Map[String, Any]) = true

    override def toString = "true"
  }

  case class FalseExpr extends LDAPExpr {
    override def apply(map: Map[String, Any]) = false

    override def toString = "false"
  }

  case class And(parts: List[LDAPExpr]) extends LDAPExpr {
    override def apply(map: Map[String, Any]) = {

      @tailrec
      def check(p: List[LDAPExpr]): Boolean = {
        p.isEmpty || (p.head(map) && check(p.tail))
      }

      check(parts)
    }

    override def toString = "(&" + parts.mkString("(", ")(", ")" + ")")
  }

  case class Or(parts: List[LDAPExpr]) extends LDAPExpr {
    override def apply(map: Map[String, Any]) = {

      @tailrec
      def check(p: List[LDAPExpr]): Boolean = {
        !p.isEmpty && (p.head(map) || check(p.tail))
      }

      check(parts)
    }

    override def toString = "(|" + parts.mkString("(", ")(", ")" + ")")
  }

  case class Not(expr: LDAPExpr) extends LDAPExpr {
    override def apply(map: Map[String, Any]) = !expr(map)

    override def toString = "(!(" + expr + "))"
  }

  private val LongPattern = """[0-9]+""".r.pattern
  private val DoublePattern = """[0-9]+\.[0-9]+""".r.pattern
  private val VersionPattern = """[0-9]+\.[0-9]+\.[0-9]+(\.[0-9a-zA-Z]+)?""".r.pattern
  // TODO make this case insensitive?
  private val BooleanPattern = """true|TRUE|false|FALSE""".r.pattern

  private final class AnyOrdered(private val underlying: Any, conversion: (String) => String = (in: String) => in) extends Ordered[AnyOrdered] {
    assert(underlying != null)

    def compare(that: AnyOrdered) = {
      def longCompare(l: Long) = {
        that.underlying match {
          case b: Boolean => throw new IllegalStateException("Not yet implemented")
          case b: Byte => l compare b
          case s: Short => l compare s
          case c: Char => l compare c
          case i: Int => l compare i
          case ll: Long => l compare ll
          case f: Float => l.toFloat compare f
          case d: Double => l.toDouble compare d
          case str: String => {
            if (LongPattern.matcher(str.trim).matches) {
              l compare str.trim.toLong
            } else if (DoublePattern.matcher(str).matches) {
              l.toDouble compare str.toDouble
            } else {
              l.toString compare str
            }
          }
        }
      }

      def floatCompare(f: Float) = {
        that.underlying match {
          case b: Boolean => throw new IllegalStateException("Not yet implemented")
          case b: Byte => f compare b.toFloat
          case s: Short => f compare s.toFloat
          case c: Char => f compare c.toFloat
          case i: Int => f compare i.toFloat
          case l: Long => f compare l.toFloat
          case ff: Float => f compare ff
          case d: Double => f.toDouble compare d
          case str: String => {
            if (LongPattern.matcher(str).matches) {
              f compare str.toFloat
            } else if (DoublePattern.matcher(str).matches) {
              f compare str.toFloat
            } else {
              f.toString compare str
            }
          }
        }
      }

      def doubleCompare(d: Double) = {
        that.underlying match {
          case b: Boolean => throw new IllegalStateException("Not yet implemented")
          case b: Byte => d compare b.toDouble
          case s: Short => d compare s.toDouble
          case c: Char => d compare c.toDouble
          case i: Int => d compare i.toDouble
          case l: Long => d compare l.toDouble
          case f: Float => d compare f.toDouble
          case dd: Double => d compare dd
          case str: String => {
            if (LongPattern.matcher(str).matches) {
              d compare str.toDouble
            } else if (DoublePattern.matcher(str).matches) {
              d compare str.toDouble
            } else {
              d.toString compare str
            }
          }
        }
      }

      def booleanCompare(b: Boolean) = {
        val c = that.underlying match {
          case bb: Boolean => bb
          // TODO not sure I like implicit conversion of numbers to booleans...
          //          case bb: Byte => if (bb == 0) false else true
          //          case s: Short => if (s == 0) false else true
          //          case c: Char => if (c == 0) false else true
          //          case i: Int => if (i == 0) false else true
          //          case l: Long => if (l == 0) false else true
          //          case f: Float => if (f == 0) false else true
          //          case dd: Double => if (dd == 0) false else true
          case str: String => {
            toBool(str)
          }
        }

        if (b) {
          if (c) {
            0
          }
          else {
            -1
          }
        }
        else {
          if (c) {
            1
          }
          else {
            0
          }
        }
      }

      def versionCompare(v: Version) = {
        that.underlying match {
          case vr: Version => v.compare(vr)
          case str: String => v.compare(Version(str))
          case other => throw new IllegalStateException("Cannot compare " + v + " to " + other)
        }
      }

      def stringCompare(str: String): Int = {
        if (LongPattern.matcher(str).matches) {
          longCompare(str.toLong)
        } else if (DoublePattern.matcher(str).matches) {
          doubleCompare(str.toDouble)
        } else if (VersionPattern.matcher(str).matches) {
          versionCompare(Version(str))
        } else if (BooleanPattern.matcher(str).matches) {
          booleanCompare(toBool(str))
        } else {
          val v1 = conversion(str)
          val v2 = that.underlying.toString
          v1.compare(v2)
        }
      }

      //      def comparableCompare(c: Comparable[_]) = {
      //        if (LongPattern.matcher(str).matches) {
      //          longCompare(str.toLong)
      //        } else if (DoublePattern.matcher(str).matches) {
      //          doubleCompare(str.toDouble)
      //        } else if (VersionPattern.matcher(str).matches) {
      //          versionCompare(Version(str))
      //        } else if (BooleanPattern.matcher(str).matches) {
      //          booleanCompare(toBool(str))
      //        } else {
      //          conversion(str).compare(that.underlying.toString)
      //        }
      //
      //      }

      underlying match {
        case b: Boolean => booleanCompare(b)
        case b: Byte => longCompare(b.toLong)
        case s: Short => longCompare(s.toShort)
        case i: Int => longCompare(i.toLong)
        case l: Long => longCompare(l)
        case f: Float => floatCompare(f)
        case d: Double => doubleCompare(d)
        case v: Version => versionCompare(v)
        case c: Char => stringCompare(c.toString)
        case str: String => stringCompare(str)
        case c: java.lang.Comparable[_] => throw new UnsupportedOperationException("Not yet implemented")
        case _ => throw new IllegalArgumentException("Uncomparable value")
      }
    }

    // TODO move to helper object
    // TODO is lowercase valid here?
    private def toBool(str: String) = java.lang.Boolean.parseBoolean(str.toLowerCase)

    override def equals(other: Any) = {
      val x = other.asInstanceOf[AnyRef]
      if (this eq x) true
      else if (x eq null) false
      else {
        x match {
          case that: AnyOrdered => try {
            this.compare(that) == 0
          }
          catch {
            case e: IllegalArgumentException => false
          }
          case _ => false
        }
      }
    }

    // for consistency implement hashCode
    // though this class is private and not currently
    // used in any maps
    override def hashCode = 41 * underlying.hashCode
  }

  private class LDAPParser extends RegexParsers {

    override val whiteSpace = """(\s)+""".r

    private var inValue = false

    override protected def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int =
      if (inValue) {
        offset
      }
      else {
        super.handleWhiteSpace(source, offset)
      }

    def expr: Parser[LDAPExpr] = simpleExpr | bracedExpr | complexExpr

    def complexExpr: Parser[LDAPExpr] = andExpr | orExpr | notExpr

    def notExpr: Parser[Not] = "(" ~ "!" ~ (bracedExpr | complexExpr) ~ ")" ^^ {
      case _ ~ _ ~ expression ~ _ => new Not(expression)
    }

    def andExpr: Parser[And] = "(" ~ "&" ~ rep((bracedExpr | complexExpr)) ~ ")" ^^ {
      case _ ~ _ ~ expressions ~ _ => new And(expressions)
    }

    def orExpr: Parser[Or] = "(" ~ "|" ~ rep((bracedExpr | complexExpr)) ~ ")" ^^ {
      case _ ~ _ ~ expressions ~ _ => new Or(expressions)
    }

    def bracedExpr: Parser[LDAPExpr] = {
      "(" ~> simpleExpr <~ ")"
    }

    def simpleExpr: Parser[LDAPExpr] = simple | trueExpr | falseExpr

    def simple: Parser[Simple] = key ~ allOp ~ {
      inValue = true
      try {
        value
      }
      finally {
        inValue = false
      }
    } ^^ {
      case x ~ allOp ~ y => {
        new Simple(x.trim, allOp, y)
      }
    }

    def trueExpr: Parser[TrueExpr] = "true" ^^ (_ => True)

    def falseExpr: Parser[FalseExpr] = "false" ^^ (_ => False)

    def allOp: Parser[Op] = eq | approx | gtoe | gt | ltoe | lt

    def eq: Parser[EqualOp] = "=" ^^ (_ => `=`)

    def approx: Parser[ApproxEqualOp] = "~=" ^^ (_ => `~=`)

    def gt: Parser[GreaterThanOp] = ">" ^^ (_ => `>`)

    def gtoe: Parser[GreaterThanOrEqualOp] = ">=" ^^ (_ => `>=`)

    def lt: Parser[LessThanOp] = "<" ^^ (_ => `<`)

    def ltoe: Parser[LessThanOrEqualOp] = "<=" ^^ (_ => `<=`)

    def wildCard: Parser[String] = WildCard

    def key: Parser[String] = """[a-zA-Z0-9#$%&._ *!|&]*""".r

    def value: Parser[String] = """[a-zA-Z0-9\*!"#$%&'+, ./:?@\^_`{|}~-]*""".r
  }

}

abstract class LDAPExpr extends (Map[String, Any] => Boolean) {
  def contains(exprFilter: (LDAPExpr => Boolean)): Boolean = {
    this match {
      case And(exprs) => exprs.exists(_.contains(exprFilter))
      case Or(exprs) => exprs.exists(_.contains(exprFilter))
      case Not(expr) => expr.contains(exprFilter)
      case s: Simple => exprFilter(s)
    }
  }

  // TODO implement walk...
  //  def walk[T](start: T)(op: (T, LDAPExpr) => T): T = {
  //    var result = start
  //    op(result, this)
  //    this.seq foreach (x => result = op(result, x))
  //    result
  //  }

  def simplify(): LDAPExpr = {
    this match {
      case And(Nil) => True
      case And(expr :: Nil) => expr.simplify

      // collapse nested ands to single level
      case And(expr :: And(nested) :: rest) => new And(expr :: nested ::: rest).simplify

      case And(True :: rest) => new And(rest).simplify
      case And(expr :: True :: rest) => new And(expr :: Nil ::: rest).simplify
      case And(expr1 :: expr2 :: True :: rest) => new And(expr1 :: expr2 :: Nil ::: rest).simplify
      case And(expr1 :: expr2 :: expr3 :: True :: rest) => new And(expr1 :: expr2 :: expr3 :: Nil ::: rest).simplify


      // catch all for any and expression that contains false
      case expr@And(parts) => if (parts.contains(False)) False else expr

      case Or(Nil) => True
      case Or(expr :: Nil) => expr.simplify

      // collapse nested ors to single level
      case Or(expr :: Or(nested) :: rest) => new Or(expr :: nested ::: rest).simplify

      case Or(False :: rest) => new Or(rest).simplify
      case Or(expr :: False :: rest) => new Or(expr :: Nil ::: rest).simplify
      case Or(expr1 :: expr2 :: False :: rest) => new Or(expr1 :: expr2 :: Nil ::: rest).simplify
      case Or(expr1 :: expr2 :: expr3 :: False :: rest) => new Or(expr1 :: expr2 :: expr3 :: Nil ::: rest).simplify

      // catch all for any or expression that contains true
      case expr@Or(parts) => if (parts.contains(True)) True else expr

      case Not(True) => False
      case Not(False) => True

      //      case a@And(Simple(x1, Gt, y1), Simple(x2, Lt, y2)) => {
      //        if (x1 == x2) {
      //          if (y1 < y2) {
      //            False
      //          }
      //          else {
      //            a
      //          }
      //        }
      //        else {
      //          a
      //        }
      //      }
      //
      //      case a@And(Simple(x1, Lt, y1), Simple(x2, Gt, y2)) => {
      //        if (x1 == x2) {
      //          if (y1 > y2) {
      //            False
      //          }
      //          else {
      //            a
      //          }
      //        }
      //        else {
      //          a
      //        }
      //      }

      // ok this is as simple as it gets...
      case simple => simple
    }

  }
}
