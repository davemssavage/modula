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

/**
 * @author David Savage
 */

import org.chronologicalthought.modula.Constants.Directives._
import collection.mutable.WeakHashMap
import org.chronologicalthought.modula._
import scala.Some
import RichTypes._

object RichTypes {
  private val reqs = new WeakHashMap[Requirement, RichRequirement]()
  private val caps = new WeakHashMap[Capability, RichCapability]()

  implicit def requirementToRichRequirement(requirement: Requirement) = {
    reqs.getOrElseUpdate(requirement, new RichRequirement(requirement))
  }

  implicit def capabilityToRichCapability(capability: Capability) = {
    caps.getOrElseUpdate(capability, new RichCapability(capability))
  }

  implicit def wireToRichWire(wire: Wire) = {
    // TODO cache?
    new RichWire(wire)
  }

  implicit def richWiringToWiring(wiring: RichWiring) = wiring.underlying

  implicit def wiringToRichWiring(underlying: Map[Part, List[Wire]]) = new RichWiring(underlying)
}

// wraps any object that defines a method directives: Map[String, Any] !!
private[impl] abstract class RichHelper[T <: {def directives : Map[String, Any]}](underlying: T) {
  protected def getDirective(name: String, default: String): String = {
    underlying.directives.get(name) match {
      case Some(str: String) => str
      case Some(other) => throw new IllegalStateException("Unexpected value of " + name + " directive " + other)
      case None => default
    }
  }
}

private[impl] class RichRequirement(underlying: Requirement) extends RichHelper(underlying) {
  lazy val isDynamic: Boolean = ResolutionDynamic == getDirective(ResolutionPolicy, ResolutionMandatory)
  lazy val isOptional: Boolean = ResolutionOptional == getDirective(ResolutionPolicy, ResolutionMandatory)
  lazy val isTransitive: Boolean = ResolutionTransitiveClosure == getDirective(ResolutionClosure, ResolutionTransitiveClosure)
  lazy val isExtension: Boolean = underlying.directives.get(ExtensionExtends).isDefined
  val namespace = underlying.namespace

  private lazy val filterExpr = underlying.directives.get(RequirementFilter) match {
    case Some(l: LDAPExpr) => Some(l.simplify()) // TODO could optimize ldap in this part too
    // TODO should this be allowed?
    // there's no optimization possible here
    // also doesn't work with mandatory attributes?
    // case Some(f: (Map[String, Any] => Boolean)) => Some(f)
    case Some(f: String) => Some(LDAPExpr(f).simplify())
    case None => None
  }

  def matches(cap: Capability): Boolean = {
    if (cap == null) {
      false
    }
    else {
      def attributesMatch: Boolean = {
        def mandatoryAttributesPresent(filter: LDAPExpr) = {
          val attrs = cap.mandatoryAttributes
          attrs.isEmpty || attrs.forall(check => {
            filter.contains(expr => {
              expr match {
                case LDAPExpr.Simple(attr, _, _) => check == attr
                case _ => false
              }
            })
          })
        }

        filterExpr match {
          case Some(filter) => filter(cap.attributes) && mandatoryAttributesPresent(filter)
          case None => cap.mandatoryAttributes.isEmpty
        }
      }

      underlying.namespace == cap.namespace && attributesMatch
    }
  }
}

private[impl] class RichCapability(underlying: Capability) extends RichHelper(underlying) {
  // TODO extract mandatory directive attributes
  val namespace = underlying.namespace

  lazy val isExtensible: Boolean = "true" == getDirective(ExtensionExtensible, null)
  lazy val mandatoryAttributes = getDirective(CapabilityMandatory, "").split("""/s*,/s*""").toSet.filterNot(_ == "")

  // bundle 1a exports foo;version 1

  // bundle 1b exports foo;version 1.5

  // bundle 2a imports foo;version[1,2)
  // bundle 2a exports bar;version=1;uses:=foo

  // bundle 2b imports foo;version[1.5,2)
  // bundle 2b exports bar;version=2;uses:=foo

  // bundle 3 imports bar;version=2
  // bundle 3 exports baz;uses:=bar

  // bundle 4 imports foo;version=[1,2)
  // bundle 4 imports baz

  // path 1a, 3->2b->x << 1b, 3->2b->1b

  // TODO make this tail recursive
  // @tailrec
  def consistent(requirement: Requirement, wiring: RichWiring, visited: Map[Part, Int] = Map.empty): Boolean = {
    def invalidWire(part: Part, wire: Wire, visited: Map[Part, Int] = Map.empty): Boolean = {
      val capability = wire.capability

      if (namespace == capability.namespace) {
        if (underlying.attributes.get(namespace) == capability.attributes.get(namespace)) {
          underlying != capability
        }
        else if (capability.directives.contains(CapabilityUses)) {
          val depth: Int = visited.getOrElse(part, 0)
          if (depth > 1) {
            // println("Stop %s -> %s".format(part, capability))
            false
          }
          else {
            val next = visited + (part -> (depth + 1))
            // println("Go %s -> %s".format(part, capability))
            capability.part.requirements.exists(!consistent(_, wiring, next))
          }
        }
        else {
          false
        }
      }
      else {
        false
      }
    }

    val result = requirement.part match {
      case Some(part) => {
        wiring.get(part) match {
          case Some(wires) => {
            !wires.exists(wire => invalidWire(part, wire, visited))
          }
          case None => {
            // println("No wires")
            true
          }
        }
      }
      case None => {
        // println("No part")
        true
      }
    }

    // println("%s : \n%s\n -> \n%s\n%s".format((if (result) "consistent" else "inconsistent"), requirement, underlying, wiring))

    result
  }
}

private[impl] object RichWiring {
  val empty: RichWiring = new RichWiring(Map.empty)
}

private[impl] class RichWiring(val underlying: Map[Part, List[Wire]]) {
  override def toString = {
    "Wiring[%s]".format(underlying)
  }
  def find(part: Part): Option[Part] = {
    underlying.keys.find(_ match {
      case CompositePart(parts) => parts.contains(part)
      case other => other == part
    })
  }

  def +(wiring: RichWiring) = {
    val tmp = new collection.mutable.HashMap[Part, List[Wire]]

    for ((part, wires) <- underlying) {
      // attempt to find part in wiring to ensure merged wires are bound to correctly
      val p = wiring.find(part).getOrElse(part)
      tmp += p -> wires.map(_.updatePart(part, p).underlying)
    }

    for ((part, wires) <- wiring.underlying) {
      tmp += part -> (wires ::: tmp.getOrElse(part, Nil))
    }

    new RichWiring(tmp.toMap.mapValues(_.toSet.toList))
  }

  def +(wire: Wire): RichWiring = {
    if (wire.requirement.isExtension) {
      // TODO it is not obvious that all extensions are merges?
      // possibly rename this - as findExtensions allows requirements that are
      // not merged extensions to be found, which will then be linked
      // in normal way...
      merge(wire)
    }
    else {
      link(wire)
    }
  }

  def -(wire: Wire): RichWiring = {
    wire.requirement.part match {
      // TODO undo composite part here?
      case Some(part) => this - (wire.capability.part -> wire) - (part -> wire)
      case None => this - (wire.capability.part -> wire)
    }
  }

  def +(part: Part) {
    val existing = underlying.getOrElse(part, Nil)
    new RichWiring(underlying + (part -> existing))
  }

  def -(part: Part): RichWiring = new RichWiring(underlying - part)

  def parts = underlying.keySet.toSet

  def unsatisfied: Traversable[Requirement] = {
    underlying.collect({
      case (part, Nil) => part.requirements
      case (part, wires) => {
        val found = wires.map(_.requirement)
        part.requirements.filterNot(found.contains(_))
      }
    }).flatten
  }

  private def merge(wire: Wire): RichWiring = {
    wire.requirement.part match {
      case Some(part) => {
        // check what we're merging with
        wire.capability.part match {
          case CompositePart(_) => {
            // TODO attempt to extend one composite part with another, not even sure what this means
            // TODO for now not supported as it leads to race conditions in ModuleProviderClassLoader when to root classloader in fragments
            this
          }
          case capPart => {
            // create a composite part that merges to parts
            val merged = new CompositePart(capPart :: part :: Nil)
            // make sure we don't create new composite parts when not necessary
            def isPartOrCompositeOfPart(mapped: (Part, List[Wire])): Boolean = {
              mapped._1 match {
                case CompositePart(parts) => parts.filter(isPartOrCompositeOfPart(_, mapped._2)).isEmpty
                case part: Part => part != capPart
              }
            }

            // get the existing wires attached to provider
            val existingWires: List[Wire] = this.underlying.getOrElse(capPart, Nil)
            // and remap wires to new composite part
            val newMap = this.underlying.filterNot(isPartOrCompositeOfPart) ++ Map(merged -> existingWires)
            // return the new map
            new RichWiring(newMap)
          }
        }
      }
      case None => throw new IllegalStateException("Attempt to extend module with empty requirement")
    }
  }

  private def link(wire: Wire): RichWiring = {
    wire.requirement.part match {
      case Some(part) => {
        if (wire.capability.part == part) {
          this + (wire.capability.part -> wire)
        }
        else {
          this + (wire.capability.part -> wire) + (part -> wire)
        }
      }
      case None => this + (wire.capability.part -> wire)
    }
  }

  private def +(partToWire: (Part, Wire)): RichWiring = {
    val part = partToWire._1
    val wire = partToWire._2

    val existing = underlying.getOrElse(part, Nil).filterNot(_ == wire)

    val wired = (wire :: existing)

    val newWires = part -> wired

    new RichWiring(underlying + newWires)
  }

  private def -(partToWire: (Part, Wire)): RichWiring = {
    val part = partToWire._1
    val wire = partToWire._2

    val existing = underlying.getOrElse(part, Nil)
    val newWires = existing filterNot (_ == wire)

    new RichWiring(underlying + (part -> newWires))
  }

  override def hashCode() = 31 * underlying.hashCode()

  override def equals(other: Any) = {
    other match {
      case v: RichWiring => {
        if (this eq v) true
        else if (v eq null) false
        else underlying == v.underlying
      }
      case _ => false
    }
  }
}

case class RichWire(underlying: Wire) {
  def updatePart(from: Part, to: Part): RichWire = {
    if (from == to) {
      this
    }
    else {
      val fromReq = underlying.requirement
      val fromCap = underlying.capability
      val toReq = fromReq.part match {
        case Some(p) => {
          if (p == from) {
            to.inheritRequirement(fromReq)
          }
          else {
            fromReq
          }
        }
        case _ => fromReq
      }

      val toCap = if (fromCap.part == from) {
        to.inheritCapability(fromCap)
      } else {
        fromCap
      }

      new RichWire(new Wire(toReq, toCap))
    }
  }
}


