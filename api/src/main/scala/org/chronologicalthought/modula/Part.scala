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

/**
 * @author David Savage
 */

// TODO javadoc
trait Part {
  // TODO doesn't part need def namespace: String for part type?
  def name: String

  def version: Version

  def capabilities: List[Capability]

  def requirements: List[Requirement]

  override def toString = super.toString + "->" + name + ":" + version

  // TODO move these helper methods to implicit RichPart type?
  // TODO investigate implicit flatten method
  def flatten: List[Part] = {
    val f = this match {
      case c@CompositePart(parts) => {
        val start = List[Part]()
        val merged: List[Part] = parts.foldLeft(start) {
          (ps, p) => {
            assert(p != null)
            assert(ps != null, "Invalid parts in " + parts)
            p.flatten ::: ps
          }
        }
        merged
      }
      case p: Part => p :: Nil
    }

    assert(f != null)
    f
  }

  def newCapability(namespace: String, attributes: Map[String, Any], directives: Map[String, Any]) = {
    new Capability(namespace, attributes, directives, this)
  }

  def newRequirement(namespace: String, attributes: Map[String, Any], directives: Map[String, Any]) = {
    new Requirement(namespace, attributes, directives, Some(this))
  }

  def inheritCapabilities(caps: List[Capability]): List[Capability] = {
    caps.map(inheritCapability(_)).toList
  }

  def inheritRequirements(reqs: List[Requirement]): List[Requirement] = {
    reqs.map(inheritRequirement(_)).toList
  }

  def inheritCapability(cap: Capability) = {
    if (cap.part == this) {
      cap
    }
    else {
      newCapability(cap.namespace, cap.attributes, cap.directives)
    }
  }

  def inheritRequirement(req: Requirement) = {
    req.part match {
      case Some(p) => {
        if (p == this) {
          req
        }
        else {
          newRequirement(req.namespace, req.attributes, req.directives)
        }
      }
      case None => newRequirement(req.namespace, req.attributes, req.directives)
    }
  }
}

object PrimordialPart extends Part {
  val name = "primordial"

  val version = Version.Empty

  val capabilities = Nil

  val requirements = Nil
}

final case class CompositePart(parts: List[Part]) extends Part {
  thisPart =>
  assert(parts != null)
  assert(!parts.isEmpty)

  val name = parts.head.name

  val version = parts.head.version

  lazy val requirements = parts.flatMap(p => inheritRequirements(p.requirements)).toList

  lazy val capabilities = parts.flatMap(p => inheritCapabilities(p.capabilities)).toList

  // TODO is equals/hashCode needed?
  override def equals(other: Any): Boolean = {
    // TODO this equals method id flaky
    val x = other.asInstanceOf[AnyRef]
    if (this eq x) true
    else if (x eq null) false
    else if (other.isInstanceOf[CompositePart]) {
      // chuckle cpo - r2d2?
      val cpo = other.asInstanceOf[CompositePart]
      return parts == cpo.parts
    }
    else {
      false
    }
  }

  override lazy val hashCode: Int = {
    parts.hashCode * 7
  }

  override def toString = parts.mkString("Composite(", ":", ")")
}