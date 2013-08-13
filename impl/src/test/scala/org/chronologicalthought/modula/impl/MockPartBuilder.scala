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

package org.chronologicalthought.modula.impl

import org.mockito.Mockito._

import org.chronologicalthought.modula.LDAPExpr._
import MockPartBuilder._
import org.chronologicalthought.modula._
import Constants.Directives._

/**
 * @author David Savage
 */

object MockPartBuilder {
  val ns = "test"

  def provides(name: String, part: Part) = Capability(ns, name, part)

  def provides(name: String, attrs: Map[String, Any], part: Part) = Capability(ns, name, attrs, part)

  def required(name: String) = Requirement(ns, name)

  def required(name: String, part: Part) = Requirement(ns, name, part)

  def required(name: String, expr: LDAPExpr) = Requirement(ns, name, expr)

  def required(name: String, expr: LDAPExpr, part: Part) = Requirement(ns, name, expr, part)

  def apply(name: String) = new MockPartBuilder(new MockPart(name, Nil, Nil))
}

class MockPart(val name: String, caps: List[Capability], val reqs: List[Requirement]) extends Part {
  val capabilities = caps.map(inheritCapability(_))
  val requirements = reqs.map(inheritRequirement(_))

  def version = Version.Empty

  def attributes = Map.empty
}

class MockPartBuilder(p: Part) {

  private val v = "version"
  private val host = "test.host"
  val part: MockPart = ensureSelfCapability(p)

  private def ensureSelfCapability(part: Part) = {
    val caps = part.capabilities
    val newCaps = if (caps.exists(_.namespace == host)) {
      caps
    } else {
      Capability(host, part.name, Map.empty, Map(ExtensionExtensible -> "true"), part) :: caps
    }
    new MockPart(part.name, newCaps, part.requirements)
  }

  def exports(name: String): MockPartBuilder = {
    val caps = provides(name, Map(v -> "0"), part) :: part.capabilities
    new MockPartBuilder(new MockPart(part.name, caps, part.requirements))
  }

  def exports(name: String, attrs: Map[String, String], mandatory: Set[String]): MockPartBuilder = {
    val attributes: Map[String, Any] = attrs ++ Map(v -> "0")
    val caps = Capability(ns, name, attributes, Map(CapabilityMandatory -> mandatory.mkString(",")), part) :: part.capabilities
    new MockPartBuilder(new MockPart(part.name, caps, part.requirements))
  }

  def exports(name: String, version: String): MockPartBuilder = {
    val caps = provides(name, Map(v -> version), part) :: part.capabilities
    new MockPartBuilder(new MockPart(part.name, caps, part.requirements))
  }

  def exports(name: String, version: String, uses: String*): MockPartBuilder = {
    val caps = Capability(ns, name, Map(v -> version), Map(CapabilityUses -> uses.mkString(",")), part) :: part.capabilities
    new MockPartBuilder(new MockPart(part.name, caps, part.requirements))
  }

  def imports(name: String): MockPartBuilder = {
    imports(name, "0")
  }

  def imports(name: String, version: String): MockPartBuilder = {
    val range = VersionRange(version)

    val min = expr(v, if (range.inclusiveMin) `>=` else `>`, range.minimum.toString)
    val max = expr(v, if (range.inclusiveMax) `<=` else `<`, range.maximum.toString)

    val ldapVersion = and(min, max)

    val reqs = required(name, ldapVersion, part) :: part.requirements
    new MockPartBuilder(new MockPart(part.name, part.capabilities, reqs))
  }

  def `extends`(name: String): MockPartBuilder = {
    val reqs = new Requirement(host, Map(host -> name), Map(RequirementFilter -> expr(host, `=`, name), ExtensionExtends -> "true"), Some(part)) :: part.requirements
    new MockPartBuilder(new MockPart(name, part.capabilities, reqs))
  }

  val build: Part = {
    part
  }
}

