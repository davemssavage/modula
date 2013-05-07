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

import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import java.util.Vector
import LDAPExpr._
import java.lang.Boolean

/**
 * @author David Savage
 */
class LDAPTest {
  @Test
  def parseEquals() {
    assertTrue(LDAPExpr("a=1")(Map("a" -> "1")))
    assertTrue(LDAPExpr("a = 1")(Map("a" -> "1")))
    assertFalse(LDAPExpr("a=1")(Map("a" -> "2")))
    assertFalse(LDAPExpr("b=1")(Map("a" -> "1")))
  }

  @Test
  def parseGreaterThanOrEquals() {
    assertTrue(LDAPExpr("a>=1")(Map("a" -> "1")))
  }

  @Test
  def parseLessThan() {
    assertTrue(LDAPExpr("a<2")(Map("a" -> "1")))
  }

  @Test
  def parseLessThanOrEquals() {
    assertTrue(LDAPExpr("a<=1")(Map("a" -> "1")))
  }

  @Test
  def parseKeyWithDots() {
    LDAPExpr("(x.y=true)")
  }

  @Test
  def parseValueWithDots() {
    LDAPExpr("(objectclass=org.foo.Bar)")
  }

  @Test
  def parseKeyAndValueWithDots() {
    LDAPExpr("(&(objectclass=org.foo.Bar)(x.y=true))")
  }

  @Test
  def parseValueWithSpaces() {
    LDAPExpr("(msg=This should work)")
  }

  @Test
  def parseEqualsList() {
    assertTrue(LDAPExpr("a=1")(Map("a" -> List("1", "2", "3"))))
    assertFalse(LDAPExpr("a=1")(Map("a" -> Nil)))
    assertFalse(LDAPExpr("a=1")(Map("a" -> List())))
  }

  @Test
  def parseEqualsArray() {
    assertTrue(LDAPExpr("a=1")(Map("a" -> Array("1", "2", "3"))))
  }

  @Test
  def parseEqualsJavaVector() {
    val v = new Vector[String]()
    v.add("1")
    v.add("2")
    v.add("3")
    assertTrue(LDAPExpr("a=1")(Map("a" -> v)))
  }

  @Test
  def parseAnyStringValues() {
    assertTrue(LDAPExpr("a=a.b.c")(Map("a" -> "a.b.c")))
  }

  @Test
  def parseNot() {
    assertFalse(LDAPExpr("(!(a=1))")(Map("a" -> "1")))
    assertTrue(LDAPExpr("(!(a=1))")(Map("a" -> "2")))
  }

  @Test
  def parseAnd() {
    assertTrue(LDAPExpr("(&(a=1)(b=1))")(Map("a" -> "1", "b" -> "1")))
    assertFalse(LDAPExpr("(&(a=1)(b=1))")(Map("a" -> "2", "b" -> "1")))
    assertFalse(LDAPExpr("(&(a=1)(b=1))")(Map("a" -> "1", "b" -> "2")))
  }

  @Test
  def parseOr() {
    assertTrue(LDAPExpr("(|(a=1)(b=1))")(Map("a" -> "1", "b" -> "2")))
    assertTrue(LDAPExpr("(|(a=1)(b=1))")(Map("a" -> "2", "b" -> "1")))
    assertFalse(LDAPExpr("(|(a=1)(b=1))")(Map("a" -> "2", "b" -> "2")))
  }

  @Test
  def parseComplex() {
    assertFalse(LDAPExpr("(! ( & ( a = 1 ) ( b = 1 ) ) )")(Map("a" -> "1", "b" -> "1")))
  }

  @Test
  def parseNesting() {
    assertTrue(LDAPExpr("(a=1)")(Map("a" -> "1")))
    assertTrue(LDAPExpr("( a=1 )")(Map("a" -> "1")))
    assertTrue(LDAPExpr(" (a=1) ")(Map("a" -> "1")))
    assertTrue(LDAPExpr(" ( a=1 ) ")(Map("a" -> "1")))
  }

  @Test(expected = classOf[IllegalArgumentException])
  // TODO better to to tolerate this but for now parsing is too complex so fail
  def parseDoubleNested() {
    LDAPExpr("((a=1))")
  }

  @Test(expected = classOf[IllegalArgumentException])
  def parseUnbalancedNested1() {
    LDAPExpr("((a=1)")
  }

  @Test(expected = classOf[IllegalArgumentException])
  def parseUnbalancedNested2() {
    LDAPExpr("(a=1))")
  }

  @Test
  def parseEqualsBoolean() {
    assertTrue(LDAPExpr("(a=true)")(Map("a" -> true)))
    assertTrue(LDAPExpr("(a=TRUE)")(Map("a" -> true)))
    // TODO not sure if this is a valid test
    assertTrue("Not sure if true=TRUE", LDAPExpr("(a=true)")(Map("a" -> "TRUE")))
  }

  @Test
  def shouldCompareVersions() {
    val map = Map("version" -> "5", "test" -> "baz")
    assertTrue(LDAPExpr("(test=baz)")(map))
    assertTrue(LDAPExpr("(version>=5)")(map))
    assertTrue(LDAPExpr("(version<=2147483647.2147483647.2147483647.Infinity)")(map))
    assertTrue(LDAPExpr("(&(test=baz)(version>=5)(version<=2147483647.2147483647.2147483647.Infinity))")(map))
  }

  @Test
  def parseEqualsIntFloat() {
    assertTrue(LDAPExpr("a=1")(Map("a" -> 1)))
    assertTrue(LDAPExpr("a=1.0")(Map("a" -> 1)))
    assertTrue(LDAPExpr("a=1")(Map("a" -> 1.0)))
    assertTrue(LDAPExpr("a=1.0")(Map("a" -> 1.0)))
    assertFalse(LDAPExpr("a=")(Map("a" -> 1)))
  }

  @Test // TODO not sure if this is a valid test
  def parseEqualsTODO() {
    assertTrue(LDAPExpr("a=1.0")(Map("a" -> "1")))
  }

  @Test
  def parseWildCard() {
    assertTrue(LDAPExpr("a=*")(Map("a" -> "1")))
    assertTrue(LDAPExpr("a=*")(Map("a" -> "2")))
    assertTrue(LDAPExpr("a=*")(Map("a" -> "-1")))
    assertTrue(LDAPExpr("a=*")(Map("a" -> "0")))
    assertTrue(LDAPExpr("a=foo*")(Map("a" -> "foobar")))
    assertTrue(LDAPExpr("a=*bar")(Map("a" -> "foobar")))
    assertTrue(LDAPExpr("a=*oba*")(Map("a" -> "foobar")))
    assertTrue(LDAPExpr("a=f*r")(Map("a" -> "foobar")))
    assertFalse(LDAPExpr("a=foo*")(Map("a" -> "wibble")))
  }

  @Test(expected = classOf[IllegalArgumentException])
  @Ignore("Temporarilly disabled whilst getting project setup")
  def failInvalidWildCard1() {
    LDAPExpr("a>*")
  }

  @Test(expected = classOf[IllegalArgumentException])
  @Ignore("Temporarilly disabled whilst getting project setup")
  def failInvalidWildCard2() {
    LDAPExpr("a>=*")
  }

  @Test(expected = classOf[IllegalArgumentException])
  @Ignore("Temporarilly disabled whilst getting project setup")
  def failInvalidWildCard3() {
    LDAPExpr("a<*")
  }

  @Test(expected = classOf[IllegalArgumentException])
  @Ignore("Temporarilly disabled whilst getting project setup")
  def failInvalidWildCard4() {
    LDAPExpr("a<=*")
  }

  @Test
  def parseGreaterThan() {
    assertTrue(LDAPExpr("a>0")(Map("a" -> "1")))
    assertFalse(LDAPExpr("a>0")(Map("a" -> "0")))
    assertFalse(LDAPExpr("a>0")(Map("a" -> "-1")))
    assertFalse(LDAPExpr("a>0")(Map("b" -> "1")))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def invalid() {
    LDAPExpr("")
  }

  @Test
  def simplifyAnd() {
    LDAPExpr("(&(a=1))").simplify match {
      case Simple("a", `=`, "1") => // ok
      case f => fail("Failed to simplify expression -> " + f)
    }

    LDAPExpr("(&(a=1)(true))").simplify match {
      case Simple("a", `=`, "1") => // ok
      case f => fail("Failed to simplify expression -> " + f)
    }

    LDAPExpr("(&(true)(a=1))").simplify match {
      case Simple("a", `=`, "1") => // ok
      case f => fail("Failed to simplify expression -> " + f)
    }

    LDAPExpr("(&(a=1)(true)(b=1))").simplify match {
      case And(List(Simple("a", `=`, "1"), Simple("b", `=`, "1"))) => // ok
      case f => fail("Failed to simplify expression -> " + f)
    }

    LDAPExpr("(&(true)(a=1)(b=1))").simplify match {
      case And(List(Simple("a", `=`, "1"), Simple("b", `=`, "1"))) => // ok
      case f => fail("Failed to simplify expression -> " + f)
    }

    LDAPExpr("(&(a=1)(b=1)(true))").simplify match {
      case And(List(Simple("a", `=`, "1"), Simple("b", `=`, "1"))) => // ok
      case f => fail("Failed to simplify expression -> " + f)
    }
  }

  @Test
  def combinations() {
    val a = LDAPExpr("(a=1)")
    val b = LDAPExpr("(b=1)")
    val c = LDAPExpr("(c=1)")
    val abc = and(a, and(b, c))
    abc match {
      case And(Simple("a", `=`, "1") :: Simple("b", `=`, "1") :: Simple("c", `=`, "1") :: Nil) => // success
      case f => fail("Failed to simplify expression -> " + f)
    }
  }

  @Test
  def contains() {
    def assertAllAttrs(filter: LDAPExpr, attrs: Set[String]) = {
      attrs.forall(check => {
        filter.contains(expr => {
          expr match {
            case Simple(attr, _, _) => check == attr
            case _ => false
          }
        })
      })
    }
    assertTrue(assertAllAttrs(LDAPExpr("(&(a=1)(b=1))"), Set("a")))
    assertTrue(assertAllAttrs(LDAPExpr("(&(a=1)(b=1))"), Set("a", "b")))
    assertTrue(assertAllAttrs(LDAPExpr("(!(&(a=1)(b=1)))"), Set("a", "b")))
    assertTrue(assertAllAttrs(LDAPExpr("(!(| (&(a=1)(c=1)) (b=1)))"), Set("a", "b")))
    assertFalse(assertAllAttrs(LDAPExpr("(b=1)"), Set("a", "b")))
    assertFalse(assertAllAttrs(LDAPExpr("(!(b=1))"), Set("a", "b")))
  }
}

