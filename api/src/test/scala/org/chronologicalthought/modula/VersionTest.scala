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

import org.junit.Test
import org.junit.Assert._
import org.chronologicalthought.modula.VersionStage._

/**
 * @author David Savage
 */

class VersionTest {

  @Test
  def parse0() {
    val v = Version("")
    assertEquals(0, v.major)
    assertEquals(0, v.minor)
    assertEquals(0, v.micro)
    assertEquals("", v.qualifier)
    assertEquals(PreRelease, v.stage)
  }

  @Test
  def parse1() {
    val v = Version("1")
    assertEquals(1, v.major)
    assertEquals(0, v.minor)
    assertEquals(0, v.micro)
    assertEquals("", v.qualifier)
    assertEquals(Release, v.stage)
  }

  @Test
  def parse2() {
    val v = Version("1.2")
    assertEquals(1, v.major)
    assertEquals(2, v.minor)
    assertEquals(0, v.micro)
    assertEquals("", v.qualifier)
    assertEquals(Release, v.stage)
  }

  @Test
  def parse3() {
    val v = Version("1.2.3")
    assertEquals(1, v.major)
    assertEquals(2, v.minor)
    assertEquals(3, v.micro)
    assertEquals("", v.qualifier)
    assertEquals(Release, v.stage)
  }

  @Test
  def parse4() {
    val v = Version("1.2.3.SP1")
    assertEquals(1, v.major)
    assertEquals(2, v.minor)
    assertEquals(3, v.micro)
    assertEquals("SP1", v.qualifier)
    assertEquals(Release, v.stage)
  }

  @Test
  def parse5() {
    val v = Version("1.2.3.")
    assertEquals(1, v.major)
    assertEquals(2, v.minor)
    assertEquals(3, v.micro)
    assertEquals("", v.qualifier)
    assertEquals(Release, v.stage)
  }

  @Test
  def parse6() {
    val v = Version("1.2.3-")
    assertEquals(1, v.major)
    assertEquals(2, v.minor)
    assertEquals(3, v.micro)
    assertEquals("", v.qualifier)
    assertEquals(PreRelease, v.stage)
  }

  @Test
  def parse7() {
    val v = Version("1.2.3-SNAPSHOT")
    assertEquals(1, v.major)
    assertEquals(2, v.minor)
    assertEquals(3, v.micro)
    assertEquals("SNAPSHOT", v.qualifier)
    assertEquals(PreRelease, v.stage)
  }

  @Test
  def interned1() {
    assertSame(Version("1"), Version("1"))
  }

  @Test
  def interned2() {
    assertSame(Version("1.2"), new Version(1, 2, 0, "", Release).intern)
  }

  @Test
  def interned3() {
    assertSame(Version("1.2.3"), new Version(1, 2, 3, "", Release).intern)
  }

  @Test
  def interned4() {
    assertSame(Version("1.0.3"), new Version(1, 0, 3, "", Release).intern)
  }

  @Test
  def interned5() {
    assertSame(Version("0.0.3"), new Version(0, 0, 3, "", Release).intern)
  }

  @Test
  def caseClass() {
    Version("1.0.0") match {
      case Version(1, 0, 0, "", Release) => // success
      case _ => fail("Failed to match version")
    }

    Version("1.0.0") match {
      case Version(1, _, _, _, _) => // success
      case _ => fail("Failed to match version")
    }

    Version("1.0.0.SP1   ") match {
      case Version(_, _, _, "SP1", _) => // success
      case _ => fail("Failed to match version")
    }

    Version("1.0.0-SNAPSHOT   ") match {
      case Version(_, _, _, _, PreRelease) => // success
      case _ => fail("Failed to match version")
    }
  }

  @Test
  def orderedStandard() {
    assertTrue(Version("1.0.0") > Version("0.0.0"))
    assertFalse(Version("1.0.0") > Version("1.0.0"))
    assertFalse(Version("1.0.0") < Version("1.0.0"))

    assertTrue(Version("1.1.0") > Version("1.0.0"))
    assertTrue(Version("1.1.1") > Version("1.1.0"))
    assertTrue(Version("1.0.0.BETA") > Version("1.0.0.ALPHA"))
    assertTrue(Version("1.0.0") > Version("1.0.0-SNAPSHOT"))
    assertTrue(Version("1.0.0.SNAPSHOT") > Version("1.0.0-SNAPSHOT"))
  }

  @Test
  def orderedZeroInfinity() {
    assertTrue(Version.Infinite > Version.Empty)
    assertTrue(Version.Empty < Version.Infinite)
    assertFalse(Version.Empty < Version.Empty)
    assertFalse(Version.Empty > Version.Empty)
    assertFalse(Version.Infinite < Version.Infinite)
    assertFalse(Version.Infinite > Version.Infinite)
  }

  @Test
  def comparableZeroInfinity() {
    assertTrue(Version.Infinite.compare(Version.Empty) > 0)
    assertTrue(Version.Infinite.compare(Version.Infinite) == 0)
    assertTrue(Version.Empty.compare(Version.Infinite) < 0)
    assertTrue(Version.Empty.compare(Version.Empty) == 0)
  }

  @Test
  def orderedInfinityAlmostInfinity() {
    val almostInfinite = new Version(Int.MaxValue, Int.MaxValue, Int.MaxValue, "almost", VersionStage.Release)
    assertTrue(Version.Infinite > almostInfinite)
    assertTrue(almostInfinite < Version.Infinite)
    assertFalse(almostInfinite < almostInfinite)
    assertFalse(almostInfinite > almostInfinite)
  }

  @Test
  def versionToString() {
    def test(str: String) {
      assertEquals(str, Version(str).toString)
    }

    test("1")
    test("1.1")
    test("1.1.1")
    test("1.0.1")
    test("1.1.1.SP1")
    test("1.0.0.SP1")
    test("1.0.0-SNAPSHOT")
  }

  @Test
  def versionToStringCollapsesRedundantInfo() {
    def test(str: String, ver: String) {
      assertEquals(str, Version(ver).toString)
    }

    test("1", "1.0")
    test("1.1", "1.1.0")
  }

  @Test
  def assertInfinity() {
    assertEquals(Release, Version.Infinite.stage)
  }

  @Test(expected = classOf[AssertionError])
  def negativeVersionsAreInvalid1() {
    Version(-1, 0, 0, "", Release)
  }

  @Test(expected = classOf[AssertionError])
  def negativeVersionsAreInvalid2() {
    Version(0, -1, 0, "", Release)
  }

  @Test(expected = classOf[AssertionError])
  def negativeVersionsAreInvalid3() {
    Version(0, 0, -1, "", Release)
  }
}
