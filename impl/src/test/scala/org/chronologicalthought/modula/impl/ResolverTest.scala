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

import FrameworkFactoryImpl._
import org.junit.Assert._
import org.mockito.Mockito._
import org.mockito.Matchers._

import org.junit.Test
import MockPartBuilder._
import org.chronologicalthought.modula._
import Constants.Directives._


/**
 * @author David Savage
 */

class ResolverTest {

  @Test
  def findResolver() {
    val framework = createFramework()
    locateResolver(framework)
  }

  @Test(expected = classOf[IllegalStateException])
  def resolveTrivialUnmatchedRequirement() {
    val environment = mock(classOf[Environment])

    val requirement = required("foo")

    when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(Nil)
    when(environment.wiring).thenReturn(Map[Part, List[Wire]]())
    when(environment.requirementFilter).thenReturn(None)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    resolver.resolve(environment, requirement)
  }

  @Test(expected = classOf[IllegalStateException])
  def resolveUnmatchedRequirement() {
    val environment = mock(classOf[Environment])

    val part = mock(classOf[Part])
    val capability = provides("foo", part)
    val requirement = required("foo", LDAPExpr("key=1"))

    when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(capability :: Nil)
    when(environment.wiring).thenReturn(Map[Part, List[Wire]]())
    when(environment.requirementFilter).thenReturn(None)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    resolver.resolve(environment, requirement)
  }

  @Test
  def resolveEmptyRequirement() {
    val environment = mock(classOf[Environment])
    when(environment.requirementFilter).thenReturn(None)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    val delta = resolver.resolve(environment, Nil).openOrThrowException("Failed to resolve empty")

    assertTrue(delta.isEmpty)
  }

  @Test(expected = classOf[AssertionError])
  def resolveNullRequirement() {
    val environment = mock(classOf[Environment])

    val framework = createFramework()

    val resolver = locateResolver(framework)

    val req: Requirement = null

    val delta = resolver.resolve(environment, req)

    assertTrue(delta.isEmpty)
  }

  @Test(expected = classOf[AssertionError])
  def resolveRequirementsContainsNull() {
    val environment = mock(classOf[Environment])
    when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(Nil)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    // TODO define Requirement apply method for this
    val reqs = new Requirement("simple", Map.empty, Map(ResolutionPolicy -> ResolutionOptional), None) :: null :: Nil

    val delta = resolver.resolve(environment, reqs)

    assertTrue(delta.isEmpty)
  }

  @Test
  def resolveSimpleRequirement() {
    val environment = mock(classOf[Environment])

    val part = new MockPartBuilder("foo").exports("foo").build
    val requirement = required("foo")
    val caps = part.capabilities

    when(environment.findExtensions(isA(classOf[Traversable[Capability]]))).thenReturn(Nil)
    when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(caps)
    when(environment.wiring).thenReturn(Map[Part, List[Wire]]())
    when(environment.requirementFilter).thenReturn(None)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    val delta = resolver.resolve(environment, requirement).openOrThrowException("Failed to resolve %s".format(requirement))

    assertFoundPartAndWire(delta, part, new Wire(requirement, part.capabilities.head))
  }

  @Test
  def resolveSimpleOptionalRequirementNoProvider() {
    val environment = mock(classOf[Environment])

    val part = mock(classOf[Part])
    // TODO define Requirement applies method for optional
    val requirement = new Requirement("simple", Map.empty, Map(ResolutionPolicy -> ResolutionOptional), None)

    when(environment.findExtensions(isA(classOf[Traversable[Capability]]))).thenReturn(Nil)
    when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(Nil)
    when(environment.wiring).thenReturn(Map[Part, List[Wire]]())
    when(environment.requirementFilter).thenReturn(None)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    val delta = resolver.resolve(environment, requirement).openOrThrowException("Failed to resolve %s".format(requirement))

    assertTrue(delta.isEmpty)
  }

  @Test
  def resolveSimpleTree() {
    val environment = mock(classOf[Environment])

    val part1 = new MockPartBuilder("part1").exports("foo") imports ("bar") imports ("baz") build
    val part2 = new MockPartBuilder("part2") exports ("bar") build
    val part3 = new MockPartBuilder("part3") exports ("baz") build
    val caps = part1.capabilities.toList ::: part2.capabilities.toList ::: part3.capabilities.toList

    when(environment.findExtensions(isA(classOf[Traversable[Capability]]))).thenReturn(Nil)
    when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(caps)
    when(environment.wiring).thenReturn(Map[Part, List[Wire]]())
    when(environment.requirementFilter).thenReturn(None)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    val root = required("foo")

    val delta = resolver.resolve(environment, root).openOrThrowException("Failed to resolve %s".format(root))

    assertFoundPartAndWire(delta, part1, new Wire(root, part1.capabilities.head))
    assertFoundPartAndWire(delta, part1, new Wire(part1.requirements.head, part3.capabilities.head))
    assertFoundPartAndWire(delta, part1, new Wire(part1.requirements.tail.head, part2.capabilities.head))
  }

  @Test
  def resolveSimpleTransitiveRequirement() {
    val environment = mock(classOf[Environment])

    val part1 = new MockPartBuilder("part1") exports ("foo") imports ("bar") build
    val part2 = new MockPartBuilder("part2") exports ("bar") build
    val caps = part1.capabilities.toList ::: part2.capabilities.toList

    when(environment.findExtensions(isA(classOf[Traversable[Capability]]))).thenReturn(Nil)
    when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(caps)
    when(environment.wiring).thenReturn(Map[Part, List[Wire]]())
    when(environment.requirementFilter).thenReturn(None)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    val root = required("foo")

    ResolverTrace.startTrace()
    val delta = resolver.resolve(environment, root).openOrThrowException("Failed to resolve %s".format(root))
    ResolverTrace.endTrace.foreach(frame => {
      frame match {
        case Start(wire, _, _) => println(wire)
        case _ => // ignore
      }
    })

    assertNotNull(delta)
    assertFalse(delta.isEmpty)

    assertFoundPartAndWire(delta, part1, new Wire(root, part1.capabilities.head))
    assertFoundPartAndWire(delta, part1, new Wire(part1.requirements.head, part2.capabilities.head))
    assertFoundPartAndWire(delta, part2, new Wire(part1.requirements.head, part2.capabilities.head))
  }

  @Test
  def resolveSimpleCircularRequirement() {
    val environment = mock(classOf[Environment])

    val part = new MockPartBuilder("foo") exports ("foo", "1") imports ("foo", "1") build
    val caps = part.capabilities

    when(environment.findExtensions(isA(classOf[Traversable[Capability]]))).thenReturn(Nil)
    when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(caps)
    when(environment.wiring).thenReturn(Map[Part, List[Wire]]())
    when(environment.requirementFilter).thenReturn(None)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    val root = required("foo", LDAPExpr("version=1"))
    val delta = resolver.resolve(environment, root).openOrThrowException("Failed to resolve %s".format(root))

    assertNotNull(delta)
    assertFalse(delta.isEmpty)

    assertFoundPartAndWire(delta, part, new Wire(part.requirements.head, part.capabilities.head))
  }


  @Test
  def resolveUsesConstraintExclusion() {
    val part1a = new MockPartBuilder("part1a").exports("foo", "1").build

    val part1b = new MockPartBuilder("part1b").exports("foo", "1.5").build

    val part2a = new MockPartBuilder("part2a").imports("foo", "[1,2)").exports("bar", "1", "foo").build

    val part2b = new MockPartBuilder("part2b").imports("foo", "[1.5,2)").exports("bar", "2", "foo").build

    val part3 = new MockPartBuilder("part3").imports("bar", "2").exports("baz", "1", "bar").build

    val part4 = new MockPartBuilder("part4").imports("foo", "[1,2)").imports("baz", "1").exports("test", "1").build

    val capabilities = part1a.capabilities.toList ::: part1b.capabilities.toList ::: part2a.capabilities.toList ::: part2b.capabilities.toList ::: part3.capabilities.toList ::: part4.capabilities.toList

    val framework = createFramework()

    val resolver = locateResolver(framework)

    val root = required("test")

    def resolve(capabilities: List[Capability]) {
      val environment = mock(classOf[Environment])
      when(environment.findExtensions(isA(classOf[Traversable[Capability]]))).thenReturn(Nil)
      when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(capabilities)
      when(environment.wiring).thenReturn(Map[Part, List[Wire]]())
      when(environment.requirementFilter).thenReturn(None)

      val delta = resolver.resolve(environment, root).openOrThrowException("Failed to resolve %s".format(root))

      assertNotNull(delta)
      assertFalse(delta.isEmpty)

      printWiring(part4 :: part3 :: part2b :: part1b :: Nil, delta)

      assertFoundPartAndWire(delta, part4, new Wire(root, part4.capabilities.head))
      assertFoundPartAndWire(delta, part4, new Wire(part4.requirements.head, part3.capabilities.head))
      assertFoundPartAndWire(delta, part4, new Wire(part4.requirements.tail.head, part1b.capabilities.head))

      assertFoundPartAndWire(delta, part3, new Wire(part4.requirements.head, part3.capabilities.head))
      assertFoundPartAndWire(delta, part3, new Wire(part3.requirements.head, part2b.capabilities.head))

      assertFoundPartAndWire(delta, part2b, new Wire(part3.requirements.head, part2b.capabilities.head))
      assertFoundPartAndWire(delta, part2b, new Wire(part2b.requirements.head, part1b.capabilities.head))

      assertFoundPartAndWire(delta, part1b, new Wire(part2b.requirements.head, part1b.capabilities.head))
    }

    resolve(capabilities)
    resolve(capabilities.reverse)
  }

  @Test
  def resolveExtension() {
    val environment = mock(classOf[Environment])

    val part1 = new MockPartBuilder("part1") exports ("foo") build
    val part2 = new MockPartBuilder("part2") `extends` ("part1") build
    val caps = part1.capabilities.toList ::: part2.capabilities.toList
    val exts = part2.requirements

    when(environment.findExtensions(isA(classOf[Traversable[Capability]]))).thenReturn(exts)
    when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(caps)
    when(environment.wiring).thenReturn(Map[Part, List[Wire]]())
    when(environment.requirementFilter).thenReturn(None)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    val root = required("foo")

    ResolverTrace.startTrace()
    val delta = resolver.resolve(environment, root).openOrThrowException("Failed to resolve %s".format(root))
    ResolverTrace.endTrace().foreach(frame => {
      frame match {
        case Start(wire, _, _) => println(wire)
        case _ => // ignore
      }
    })

    assertNotNull(delta)
    assertEquals(delta.toString(), 1, delta.size)
    val part2Wire = delta.toList(0)
    assertEquals(1, part2Wire._2.size)
    part2Wire._1 match {
      case c@CompositePart(parts) => {
        assertFoundPartAndWire(delta, c, new Wire(root, c.capabilities.head))
        // TODO check parts
      }
      case other => fail("Unexpected part type " + other)
    }
  }

  @Test
  def resolveMandatoryAttributes() {
    val environment = mock(classOf[Environment])

    val part = new MockPartBuilder("foo").exports("foo", Map("bar" -> "0"), Set("bar")).build
    val requirement = required("foo", LDAPExpr("(bar=*)"))
    val caps = part.capabilities

    when(environment.findExtensions(isA(classOf[Traversable[Capability]]))).thenReturn(Nil)
    when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(caps)
    when(environment.wiring).thenReturn(Map[Part, List[Wire]]())
    when(environment.requirementFilter).thenReturn(None)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    val delta = resolver.resolve(environment, requirement).openOrThrowException("Failed to resolve %s".format(requirement))

    assertFoundPartAndWire(delta, part, new Wire(requirement, part.capabilities.head))
  }

  @Test(expected = classOf[IllegalStateException])
  def resolveMandatoryAttributesNotMatched() {
    val environment = mock(classOf[Environment])

    val part = new MockPartBuilder("foo").exports("foo", Map("bar" -> "0"), Set("bar")).build
    val requirement = required("foo")
    val caps = part.capabilities

    when(environment.findExtensions(isA(classOf[Traversable[Capability]]))).thenReturn(Nil)
    when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(caps)
    when(environment.wiring).thenReturn(Map[Part, List[Wire]]())
    when(environment.requirementFilter).thenReturn(None)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    resolver.resolve(environment, requirement)
  }

  def printWiring(parts: List[Part], delta: Map[Part, Traversable[Wire]]) {
    def printWires(part: Part, wires: Traversable[Wire]) {
      println(part + "{")
      wires.foreach(wire => {
        println("  wire {")
        println("    requirement=" + wire.requirement)
        println("    capability=" + wire.capability)
        println("  }")
      })
      println("}")
    }

    for (part <- parts) {
      delta.get(part) match {
        case Some(wires) => printWires(part, wires)
        case None => println(part + "-> UNWIRED")
      }
    }

    Console.flush
  }

  private def assertFoundPartAndWire(delta: Map[Part, Traversable[Wire]], part: Part, wire: Wire) {
    val wiring = delta.getOrElse(part, throw new AssertionError("Failed to resolve " + part + "\nfound:\n" + delta.keys))
    assertTrue("Expected " + wire + " found:\n" + wiring.mkString("\n"), wiring.exists(_ == wire))
  }

  private def locateResolver(framework: Framework): Resolver = {
    framework.context.anyService(classOf[Resolver]).openOr(throw new AssertionError("Expected resolver registered"))
  }

  private def createFramework() = {
    val framework = newFramework()
    val context = framework.context
    framework.start()
    framework
  }
}
