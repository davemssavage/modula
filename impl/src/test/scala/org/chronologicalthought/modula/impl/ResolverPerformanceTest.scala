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
import org.chronologicalthought.modula.{Capability, Environment, Framework, LDAPExpr, Part, Requirement, Resolver, Wire}
import collection.mutable.ArrayBuffer
import MockPartBuilder._


/**
 * @author David Savage
 */
class ResolverPerformanceTest {

  @Test
  def resolveSimpleTransitiveRequirement() {
    val environment = mock(classOf[Environment])

    // TODO causes stack overflow greater than this - needs fixing
    val count = 400
    val parts = buildParts(count)
    val capabilities = parts.flatMap(_.capabilities)

    when(environment.findExtensions(isA(classOf[Traversable[Capability]]))).thenReturn(Nil)
    when(environment.findProviders(isA(classOf[Traversable[Requirement]]))).thenReturn(capabilities)
    when(environment.wiring).thenReturn(Map[Part, Traversable[Wire]]())
    when(environment.requirementFilter).thenReturn(None)

    val framework = createFramework()

    val resolver = locateResolver(framework)

    val root = required("cap" + 1)

    val delta = resolver.resolve(environment, root)

    assertNotNull(delta)
    assertFalse(delta.isEmpty)
    assertEquals(count, delta.size)
  }

  private def buildParts(num: Int): List[Part] = {
    val parts = new ArrayBuffer[Part]

    for (i <- 1 until num) {
      parts += new MockPartBuilder("part_" + i).exports("cap" + i).imports("cap" + (i + 1)).build
    }

    parts += new MockPartBuilder("part_" + num).exports("cap" + num).build

    parts.toList
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
