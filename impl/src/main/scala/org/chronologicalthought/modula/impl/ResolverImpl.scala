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

import annotation.tailrec
import util.control.Breaks._
import collection.mutable.ArrayBuffer
import org.chronologicalthought.modula._
import org.chronologicalthought.modula.impl.RichTypes._

/**
 * @author David Savage
 */
class ResolverImpl extends Resolver {
  def resolve(environment: Environment, requirements: List[Requirement]): Box[Map[Part, List[Wire]]] = {
    assert(environment != null, "Environment cannot be null")
    assert(requirements != null, "Requirements cannot be null")
    assert(!requirements.exists(_ == null), "Requirements cannot contain null")

    internalResolve(environment, requirements) match {
      case Resolved(wiring) => {
        // ok successfully resolved top level requirements
        // now try and attach extensions
        // TODO possibly policy based - eager attach extensions?
        attachExtensions(wiring, environment) match {
          case Resolved(result) => Full(result)
          case PartialResolution(_) | PartialFailure(_) => Failure(throw new IllegalStateException("No valid extensions for " + requirements))
          case Failed(msg, _) => Failure(throw new IllegalStateException(msg))
        }
      }
      case PartialResolution(_) | PartialFailure(_) => Failure(throw new IllegalStateException("No valid resolutions for " + requirements))
      case Failed(msg, _) => Failure(throw new IllegalStateException(msg))
    }
  }

  private def internalResolve(environment: Environment, allRequirements: Traversable[Requirement], visited: Set[Part] = Set.empty): Resolution = {
    val requirements = filterRequirements(environment, allRequirements)

    if (requirements.isEmpty) {
      NoOp
    }
    else {
      val ticker = new Ticker

      val result = doResolve(environment, requirements, visited, ticker)

      if (ticker.ticked) {
        result
      }
      else {
        if (requirements.exists(!_.isOptional)) {
          // TODO why empty here?
          new Failed("No capabilities for " + requirements, RichWiring.empty)
        }
        else {
          NoOp
        }
      }
    }
  }

  private def filterRequirements(environment: Environment, requirements: Traversable[Requirement]) = {
    if (requirements.isEmpty) {
      Nil
    }
    else {
      environment.requirementFilter match {
        case Some(filter) => requirements.filter {
          req => filter(req.directives)
        }
        case None => requirements
      }
    }
  }

  private def attachExtensions(wiring: RichWiring, environment: Environment): Resolution = {
    val parts = wiring.parts.flatMap(_.flatten).toSet

    val extendibles = for {
      part <- parts
      cap <- part.capabilities
      if cap.isExtensible
    } yield cap

    if (!extendibles.isEmpty) {
      val potential = new PotentialEnvironment(wiring, environment)
      val extensions = potential.findExtensions(extendibles)
      val extended = internalResolve(potential, extensions.toTraversable, parts)

      extended match {
        case Resolved(ext) => new Resolved(wiring + ext)
        case other => {
          if (extensions.forall(_.isOptional)) {
            new Resolved(wiring)
          }
          else {
            new Failed("No providers found for " + extensions, wiring)
          }
        }
      }
    }
    else {
      new Resolved(wiring)
    }
  }

  private implicit def environmentToRichEnvironment(env: Environment): RichEnvironment = {
    new RichEnvironment(env)
  }

  private class RichEnvironment(env: Environment) {
    def without(cap: Capability): Environment = {
      new Environment {
        def findExtensions(capabilities: Traversable[Capability]) = env.findExtensions(capabilities)

        def wiring = env.wiring

        def findProviders(requirements: Traversable[Requirement]) = env.findProviders(requirements).filter(_ != cap)

        def requirementFilter = env.requirementFilter
      }
    }
  }

  private def doResolve(environment: Environment, requirements: Traversable[Requirement], visited: Set[Part], ticker: Ticker): Resolution = {
    // TODO think start should include environment wiring? causes failure in extensions if passed through
    //val start = new PartialResolution(new RichWiring(environment.wiring))
    var resolution: Resolution = new PartialResolution(RichWiring.empty)

    for (next <- requirements) {
      resolution = resolution match {
        case current@PartialResolution(wiring) => {
          ticker.tick()
          matchProvider(environment, current, next, visited)
        }
        case Resolved(wiring) => {
          ticker.tick()
          matchProvider(environment, new PartialResolution(wiring), next, visited)
        }
        case other => other
      }
    }

    resolution match {
      case PartialResolution(wiring) => new Resolved(wiring)
      case PartialFailure(invalid) => doResolve(environment without invalid, requirements, visited, ticker.reset())
      case other => other
    }
  }

  private def matchProvider(environment: Environment, start: PartialResolution, requirement: Requirement, visited: Set[Part]): Resolution = {
    var resolution: Resolution = start

    val capabilities = environment.findProviders(requirement :: Nil)

    val ticker = new Ticker

    breakable {
      for (capability <- capabilities) {
        if (ticker.ticked) {
          break()
        }

        def check(current: PartialResolution) = {
          if (requirement.matches(capability)) {
            if (capability.consistent(requirement, current.wiring)) {
              val wire = new Wire(requirement, capability)
              val added = addWire(environment, wire, current, visited)
              added match {
                case Failed(_, _) => new PartialFailure(capability)
                case r: Resolved => {
                  ticker.tick()
                  r
                }
                case other => other
              }
            }
            else {
              new PartialFailure(capability)
            }
          }
          else {
            current
          }
        }

        resolution = resolution match {
          case PartialResolution(_) | PartialFailure(_) => check(start)
          case other => other
        }
      }
    }

    if (ticker.ticked) {
      resolution
    }
    else {
      if (requirement.isOptional) {
        start
      }
      else {
        new Failed("No valid providers found for " + requirement, start.wiring)
      }
    }
  }

  private def addWire(environment: Environment, wire: Wire, resolution: PartialResolution, visited: Set[Part]): Resolution = {
    ResolverTrace.traceAddWireStart(wire, resolution.wiring, visited)

    val requirement = wire.requirement
    val capability = wire.capability

    // println("Adding %s\n to \n%s".format(wire, wiring))

    val potentialWiring = resolution.wiring + wire

    val transitiveResolution: Resolution = {
      val flat = capability.part.flatten
      if (requirement.isTransitive && isNewParts(visited, flat)) {
        val newEnvironment = new PotentialEnvironment(potentialWiring, environment)
        internalResolve(newEnvironment, capability.part.requirements, visited ++ flat)
      } else {
        NoOp
      }
    }

    transitiveResolution match {
      case Resolved(transitive) => {
        val mergedWires = potentialWiring + transitive
        ResolverTrace.traceAddWireEnd(wire, mergedWires, visited)
        // TODO changing this to resolved inverts uses fixes???
        new Resolved(mergedWires)
      }
      case PartialResolution(transitive) => {
        val mergedWires = potentialWiring + transitive
        ResolverTrace.traceAddWireEnd(wire, mergedWires, visited)
        new Resolved(mergedWires)
      }
      case other => other
    }
  }

  private def isNewParts(visited: Set[Part], parts: List[Part]) = {
    parts.forall(visited.contains(_) == false)
  }
}

private sealed abstract class Resolution

private case class Resolved(wiring: RichWiring) extends Resolution

private case class PartialResolution(wiring: RichWiring) extends Resolution

private case class PartialFailure(invalid: Capability) extends Resolution

private case class PartialFailure2(msg: String) extends Resolution

private case class Failed(msg: String, wiring: RichWiring) extends Resolution

private object NoOp extends Resolved(RichWiring.empty) {
  def unapply(resolution: Resolution): Option[Resolution] = resolution match {
    case Resolved(RichWiring.empty) => Some(NoOp)
    case _ => None
  }
}

private class Ticker {
  var ticked = false;

  def tick() {
    ticked = true
  }

  def reset() = {
    ticked = false
    this
  }
}

object ResolverTrace {
  val traceLocal = new ThreadLocal[ArrayBuffer[Frame]]

  def startTrace() {
    traceLocal.set(new ArrayBuffer)
  }

  def traceAddWireStart(wire: Wire, wiring: Map[Part, List[Wire]], visited: Set[Part]) {
    val buf = traceLocal.get
    if (buf != null) {
      buf += new Start(wire, wiring, visited)
    }
  }

  def traceAddWireEnd(wire: Wire, wiring: Map[Part, List[Wire]], visited: Set[Part]) {
    val buf = traceLocal.get
    if (buf != null) {
      buf += new End(wire, wiring, visited)
    }
  }

  def endTrace(): List[Frame] = {
    val buf = traceLocal.get
    traceLocal.set(null)
    if (buf == null) Nil
    else {
      buf.toList
    }
  }
}

abstract class Frame

case class Start(wire: Wire, wiring: Map[Part, List[Wire]], visited: Set[Part]) extends Frame {
  override def toString = {
    "Start[" + wire + "->" + wiring + "->" + visited + "]"
  }
}

case class End(wire: Wire, wiring: Map[Part, List[Wire]], visited: Set[Part]) extends Frame {
  override def toString = {
    "End[" + wire + "->" + wiring + "->" + visited + "]"
  }
}

private case class PotentialEnvironment(wiring: Map[Part, List[Wire]], parent: Environment) extends Environment {
  assert(wiring != null)
  assert(parent != null)

  val partsBeingWired = wiring.keys
  val capabilitiesFromPartsBeingWired = partsBeingWired.flatMap(_.capabilities)
  val requirementsFromPartsBeingWired = partsBeingWired.flatMap(_.requirements)
  val underlyingEnvironment = findRootEnvironment(parent)

  @tailrec
  private def findRootEnvironment(environment: Environment): Environment = {
    environment match {
      case PotentialEnvironment(_, env) => findRootEnvironment(env)
      case e => e
    }
  }

  def requirementFilter = underlyingEnvironment.requirementFilter

  def findProviders(requirements: Traversable[Requirement]) = {
    assert(requirements != null)

    val matched = capabilitiesFromPartsBeingWired.filter {
      cap => requirements.exists(_.matches(cap))
    }.iterator

    new Iterable[Capability] {
      def iterator = new PotentialCapabilityIterator(matched, requirements.toSet)
    }
  }

  def findExtensions(capabilities: Traversable[Capability]) = {
    assert(capabilities != null)

    val matched = requirementsFromPartsBeingWired.filter {
      req => capabilities.exists(
        cap => {
          val m = req.matches(cap)
          if (m) {
            // println("Matched " + cap + "->" + req)
          }
          m
        }
      )
    }.iterator

    new Iterable[Requirement] {
      def iterator = new PotentialRequirementIterator(matched, capabilities)
    }
  }

  private[impl] class PotentialCapabilityIterator(candidates: Iterator[Capability], requirementsBeingResolved: Set[Requirement]) extends Iterator[Capability] {
    def next() = {
      if (candidates.hasNext)
        candidates.next()
      else {
        val p = environmentCapabilities()
        nextFromEnvironment(p, true) match {
          case Some(c) => c
          case None => throw new NoSuchElementException
        }
      }
    }

    def hasNext = {
      if (candidates.hasNext) true
      else {
        val p = environmentCapabilities()
        nextFromEnvironment(p, false) match {
          case Some(_) => true
          case None => false
        }
      }
    }

    private var provided: Option[Iterator[Capability]] = None

    private def environmentCapabilities() = {
      provided match {
        case Some(iterator) => iterator
        case None => {
          val p = underlyingEnvironment.findProviders(requirementsBeingResolved).toIterator
          provided = Some(p)
          p
        }
      }
    }

    private var n: Option[Capability] = None

    private def nextFromEnvironment(p: Iterator[Capability], read: Boolean): Option[Capability] = {
      n match {
        case s@Some(c) => {
          if (read) n = None
          s
        }
        case None => {
          @tailrec
          def findNext(): Option[Capability] = {
            if (p.hasNext) {
              val cap = p.next()
              if (requirementsBeingResolved.exists(_.matches(cap))) Some(cap) else findNext()
            }
            else {
              None
            }
          }

          n = findNext()
          n
        }
      }
    }
  }

  private[impl] class PotentialRequirementIterator(candidates: Iterator[Requirement], capabilitiesBeingResolved: Traversable[Capability]) extends Iterator[Requirement] {
    def next() = {
      if (candidates.hasNext)
        candidates.next()
      else {
        val p = environmentRequirements()
        nextFromEnvironment(p, true) match {
          case Some(c) => c
          case None => throw new NoSuchElementException
        }
      }
    }

    def hasNext = {
      if (candidates.hasNext) true
      else {
        val p = environmentRequirements()
        nextFromEnvironment(p, false) match {
          case Some(_) => true
          case None => false
        }
      }
    }

    private var provided: Option[Iterator[Requirement]] = None

    private def environmentRequirements() = {
      provided match {
        case Some(iterator) => iterator
        case None => {
          val p = underlyingEnvironment.findExtensions(capabilitiesBeingResolved).toIterator
          provided = Some(p)
          p
        }
      }
    }

    private var n: Option[Requirement] = None

    private def nextFromEnvironment(p: Iterator[Requirement], read: Boolean): Option[Requirement] = {
      n match {
        case s@Some(r) => {
          if (read) n = None
          s
        }
        case None => {
          @tailrec
          def findNext(): Option[Requirement] = {
            if (p.hasNext) {
              val req = p.next()
              if (capabilitiesBeingResolved.exists(req.matches(_))) Some(req) else findNext()
            }
            else {
              None
            }
          }

          n = findNext()
          n
        }
      }
    }
  }

}
