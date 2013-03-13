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
import org.mockito.Mockito._

/**
 * @author David Savage
 */

class CompositePartTest {

  class MockPart(val name: String) extends Part {
    def version = Version.Empty

    def attributes = Map.empty

    def capabilities = Nil

    def requirements = Nil
  }

  @Test
  def flatten() {
    val p1 = new MockPart("1")
    val p2 = new MockPart("2")
    val p3 = new MockPart("3")

    val c1 = new CompositePart(p1 :: p2 :: Nil)
    val c2 = new CompositePart(c1 :: p3 :: Nil)

    val parts = c2.flatten

    assertSame(p3, parts.head)
    assertSame(p2, parts.tail.head)
    assertSame(p1, parts.tail.tail.head)
  }

}
