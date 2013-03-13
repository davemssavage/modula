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

/**
 * @author David Savage
 */

class VersionRangeTest {
  @Test
  def majorOneToTwoExclusive() {
    val range = VersionRange("[1.0.0,2.0.0)")
    assertTrue(range(Version("1.0.0")))
    assertTrue(range(Version("1.9.9.SP1")))
    assertFalse(range(Version("1.0.0-SNAPSHOT")))
    assertFalse(range(Version("0.9.9")))
    assertFalse(range(Version("2.0.0-SNAPSHOT")))
    assertFalse(range(Version("2")))
  }

  @Test
  def majorOneToTwoExclusivePreRelease() {
    val range = VersionRange("[1.0.0,2.0.0)", VersionStage.PreRelease)
    assertTrue(range(Version("1.0.0")))
    assertTrue(range(Version("1.9.9.SP1")))
    assertTrue(range(Version("1.0.0-SNAPSHOT")))
    assertFalse(range(Version("0.9.9")))
    assertFalse(range(Version("2.0.0-SNAPSHOT")))
    assertFalse(range(Version("2")))
  }

  @Test
  def identityRange() {
    val range = VersionRange("[1,1]")
    assertTrue(range(Version("1.0.0")))
    assertFalse(range(Version("1.0.0-")))
    assertFalse(range(Version("1.0.0.a")))
  }

  @Test
  def identityRangePreRelease() {
    val range = VersionRange("[1,1]", VersionStage.PreRelease)
    assertTrue(range(Version("1.0.0")))
    assertTrue(range(Version("1.0.0-")))
    assertFalse(range(Version("0.9.0")))
    assertFalse(range(Version("1.0.0.a")))
  }

  @Test
  def majorOneToTwoInclusive() {
    val range = VersionRange("[1.0.0,2.0.0]")
    assertTrue(range(Version("1.0.0")))
    assertTrue(range(Version("2")))
    assertTrue(range(Version("2.0.0-SNAPSHOT")))
    assertFalse(range(Version("0.9.9")))
    assertFalse(range(Version("2.0.0.SP1")))
  }

  @Test
  def majorOneToInfinity() {
    val range = VersionRange("1.0.0")
    assertTrue(range(Version("1.0.0")))
    assertTrue(range(Version("2.0.0")))
    assertTrue(range(Version.Infinite))
  }

  @Test
  def rangeToString() {
    def assertRangeToString(v: String) {
      assertEquals(v, VersionRange(v).toString)
    }

    assertRangeToString("[1,2)")
    assertRangeToString("[1,2]")
    assertRangeToString("1")
  }

  @Test
  def ldapCompareVersion() {
    val ldap = VersionRange("[1.0.0,2.0.0)").toLDAP("v")
    assertTrue(ldap(Map("v" -> Version("1.0.0"))))
    assertTrue(ldap(Map("v" -> Version("1.10.99"))))
    assertFalse(ldap(Map("v" -> Version("0.9.0"))))
    assertFalse(ldap(Map("v" -> Version("2.0.0"))))
  }

  @Test
  def ldapCompareString() {
    val ldap1 = VersionRange("[1.0.0,2.0.0)").toLDAP("v")
    assertTrue(ldap1(Map("v" -> "1.0.0")))
    assertTrue(ldap1(Map("v" -> "1.10.99")))
    assertFalse(ldap1(Map("v" -> "0.9.0")))
    assertFalse(ldap1(Map("v" -> "2.0.0")))


    val ldap2 = VersionRange("1.10.0").toLDAP("v")
    assertTrue(ldap2(Map("v" -> "1.10")))
    assertFalse(ldap2(Map("v" -> "1.9.0")))
    assertFalse(ldap2(Map("v" -> "1.9.0.SNAPSHOT")))

    //    val ldap3 = VersionRange("1.10.0").toLDAP("v")
    //    assertTrue(ldap3(Map("v" -> "1.10.1")))
    //    assertFalse(ldap3(Map("v" -> "1.9.0.SNAPSHOT")))
  }

  @Test(expected = classOf[IllegalArgumentException])
  def invalidInput1() {
    VersionRange("foo")
  }

  @Test(expected = classOf[IllegalArgumentException])
  def invalidInput2() {
    VersionRange("1,2")
  }

  @Test(expected = classOf[IllegalArgumentException])
  def checkMinimumGreaterThanMaximumFails() {
    VersionRange("[2,1)")
  }
}
