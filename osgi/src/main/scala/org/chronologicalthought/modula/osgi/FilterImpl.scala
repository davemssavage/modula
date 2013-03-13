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

package org.chronologicalthought.modula.osgi

import java.{util => ju}
import org.chronologicalthought.modula.LDAPExpr
import org.chronologicalthought.modula.LDAPExpr._
import org.osgi.framework._
import collection.JavaConversions._
import FilterImpl._

/**
 * @author David Savage
 */

class FilterImpl(expr: LDAPExpr) extends Filter {

  private lazy val lcExpr = toLowerCase(simpleExpr)
  private lazy val simpleExpr = expr.simplify()

  // TODO test null param - assume false is correct response here?
  def `match`(reference: ServiceReference[_]): Boolean = {
    var tmp: Map[String, AnyRef] = Map.empty

    for (key <- reference.getPropertyKeys) {
      tmp += key.toLowerCase -> reference.getProperty(key)
    }

    lcExpr(tmp)
  }

  /**
   * Filter using a Dictionary with case insensitive key lookup. This Filter is executed using the specified
   * Dictionary's keys and values. The keys are looked up in a case insensitive manner.
   *
   * @param dictionary - The Dictionary whose key/value pairs are used in the match.
   *
   * @returns true if the Dictionary's values match this filter; false otherwise.
   *
   * @throws java.lang.IllegalArgumentException - If dictionary contains case variants of the same key name.
   */
  // TODO throw/test ISE
  def `match`(dictionary: ju.Dictionary[String, _]): Boolean = {
    val check = nonNull(dictionary)
    var tmp: Map[String, Any] = Map.empty

    for (key <- check.keys) {
      tmp += key.toLowerCase -> check.get(key)
    }

    lcExpr(tmp)
  }

  def matchCase(dictionary: ju.Dictionary[String, _]): Boolean = {
    val check = nonNull(dictionary)
    var tmp: Map[String, Any] = Map.empty

    for (key <- check.keys) {
      tmp += key -> check.get(key)
    }

    simpleExpr(tmp)
  }

  def matches(map: ju.Map[String, _]): Boolean = {
    val check = nonNull(map)
    var tmp: Map[String, Any] = Map.empty

    for (entry <- check.entrySet) {
      tmp += entry.getKey -> entry.getValue
    }

    simpleExpr(tmp)
  }

  override def toString = expr.toString

  override def equals(other: Any) = {
    try {
      val o = other.asInstanceOf[AnyRef]
      if (this eq o) true
      else if (this eq null) false
      else toString == o.toString
    }
    catch {
      case e: ClassCastException => false
    }
  }

  override lazy val hashCode = {
    7 * toString.hashCode
  }
}

object FilterImpl {
  def toLowerCase(expr: LDAPExpr): LDAPExpr = {
    expr match {
      case Simple(key, op, value) => new Simple(key.toLowerCase, op, value)
      case And(exprs) => new And(exprs.map(toLowerCase(_)))
      case Or(exprs) => new Or(exprs.map(toLowerCase(_)))
      case Not(expr) => new Not(toLowerCase(expr))
      case other => other
    }
  }

  def nonNull(map: ju.Map[String, _]) = if (map == null) ju.Collections.emptyMap() else map

  def nonNull(dictionary: ju.Dictionary[String, _]) = if (dictionary == null) new ju.Hashtable[String, AnyRef]() else dictionary
}
