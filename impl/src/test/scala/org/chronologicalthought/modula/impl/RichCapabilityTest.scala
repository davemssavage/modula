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

import org.chronologicalthought.modula.{Part, Wire, Capability}
import org.junit.Test
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

/**
 * User: dave
 */
class RichCapabilityTest {
  val part1a = MockPartBuilder("part1a").exports("foo", "1").build
  val part1b = MockPartBuilder("part1b").exports("foo", "1.5").build
  val part2 = MockPartBuilder("part2b").imports("foo", "[1,2)").build

  @Test
  def testConsistent() {
    val wires: Map[Part, List[Wire]] = Map(part2 -> List(new Wire(part2.requirements.head, part1a.capabilities.head)))
    val wiring = new RichWiring(wires)
    assertTrue(new RichCapability(part1a.capabilities.head).consistent(part2.requirements.head, wiring))
  }

  @Test
  def testInconsistent() {
    val wires: Map[Part, List[Wire]] = Map(part2 -> List(new Wire(part2.requirements.head, part1a.capabilities.head)))
    val wiring = new RichWiring(wires)
    assertFalse(new RichCapability(part1b.capabilities.head).consistent(part2.requirements.head, wiring))
  }

  @Test
  def testConsistent2() {
    val wires: Map[Part, List[Wire]] = Map(part2 -> List(new Wire(part2.requirements.head, part1b.capabilities.head)))
    val wiring = new RichWiring(wires)
    assertTrue(new RichCapability(part1b.capabilities.head).consistent(part2.requirements.head, wiring))
  }

  @Test
  def testInconsistent2() {
    val wires: Map[Part, List[Wire]] = Map(part2 -> List(new Wire(part2.requirements.head, part1b.capabilities.head)))
    val wiring = new RichWiring(wires)
    assertFalse(new RichCapability(part1a.capabilities.head).consistent(part2.requirements.head, wiring))
  }
}
