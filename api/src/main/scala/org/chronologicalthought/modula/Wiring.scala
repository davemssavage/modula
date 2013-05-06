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

import Constants.Directives._

/**
 * @author David Savage
 */
object Capability {
  def apply(namespace: String, name: String, part: Part) = {
    new Capability(namespace, Map(namespace -> name), Map.empty, part)
  }

  def apply(namespace: String, name: String, attributes: Map[String, Any], part: Part) = {
    new Capability(namespace, attributes + (namespace -> name), Map.empty, part)
  }

  def apply(namespace: String, name: String, attributes: Map[String, Any], directives: Map[String, Any], part: Part) = {
    new Capability(namespace, attributes + (namespace -> name), directives, part)
  }
}

case class Capability(namespace: String, attributes: Map[String, Any], directives: Map[String, Any], part: Part) {
  assert(namespace != null)
  assert(part != null)
  assert(attributes != null)
  assert(directives != null)

  override def toString = {
    val buf = new StringBuilder
    buf append "Capability["
    buf append namespace
    buf append ":"
    buf append attributes
    buf append ":"
    buf append directives
    buf append ":"
    buf append part
    buf append "]"
    buf.toString
  }
}

object Requirement {
  def apply(namespace: String, name: String) = {
    // map namespace to name as it is easier to debug if these values are present than nested in a filter
    new Requirement(namespace, Map(namespace -> name), filterNamed(namespace, name), None)
  }

  def apply(namespace: String, name: String, part: Part) = {
    // map namespace to name as it is easier to debug if these values are present than nested in a filter
    new Requirement(namespace, Map(namespace -> name), filterNamed(namespace, name), Some(part))
  }

  def apply(namespace: String, name: String, filter: LDAPExpr) = {
    // map namespace to name as it is easier to debug if these values are present than nested in a filter
    new Requirement(namespace, Map(namespace -> name), filterNamedAnd(namespace, name, filter), None)
  }

  def apply(namespace: String, name: String, filter: LDAPExpr, part: Part) = {
    // map namespace to name as it is easier to debug if these values are present than nested in a filter
    new Requirement(namespace, Map(namespace -> name), filterNamedAnd(namespace, name, filter), Some(part))
  }

  private def filterNamed(namespace: String, name: String): Map[String, Any] = {
    import LDAPExpr._

    val f = expr(namespace, `=`, name)

    Map(RequirementFilter -> f)
  }

  private def filterNamedAnd(namespace: String, name: String, filter: LDAPExpr): Map[String, Any] = {
    import LDAPExpr._

    val f = and(expr(namespace, `=`, name), filter)

    Map(RequirementFilter -> f)
  }
}

case class Requirement(namespace: String, attributes: Map[String, Any], directives: Map[String, Any], part: Option[Part]) {
  assert(namespace != null)
  assert(part != null)
  assert(attributes != null)
  assert(directives != null)

  //  def apply(cap: Capability): Boolean = {
  //    if (cap == null) {
  //      false
  //    }
  //    else {
  //      def attributesMatch: Boolean = {
  //        directives.get(RequirementFilter) match {
  //          case Some(f: (Map[String, Any] => Boolean)) => f(cap.attributes)
  //          case Some(f: String) => LDAPExpr(f)(cap.attributes)
  //          case None => true
  //        }
  //      }
  //
  //      namespace == cap.namespace && attributesMatch
  //    }
  //  }

  override def toString = {
    val buf = new StringBuilder
    buf append "Requirement["
    buf append namespace
    buf append ":"
    buf append attributes
    buf append ":"
    buf append directives
    buf append ":"
    buf append part
    buf append "]"
    buf.toString()
  }

}

case class Wire(requirement: Requirement, capability: Capability) {
  override def toString = "Wire {\n  requirement=" + requirement + "\n  capability=" + capability + "\n}"
}
