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

import org.chronologicalthought.modula.VersionStage._

/**
 * @author David Savage
 */

object VersionRange {
  val All = VersionRange("0")

  def apply(str: String): VersionRange = {
    apply(str, Release)
  }

  def apply(str: String, stage: VersionStage): VersionRange = {
    buildVersionRange(str.trim(), stage)
  }

  private def buildVersionRange(str: String, stage: VersionStage) = {
    try {
      if ("""[\[(].*[)\]]""".r.findAllIn(str).isEmpty) {
        new VersionRange(true, Version(str), Version.Infinite, true)
      }
      else {
        var s = str
        val incMin = s.startsWith("[")

        s = s.dropWhile(s => s == '[' || s == '(')

        val min = s.takeWhile(_ != ',')

        s = s.drop(min.length + 1)

        val max = s.takeWhile(s => !(s == ']' || s == ')'))

        val incMax = s.endsWith("]")

        var vmax = Version(max)
        var vmin = Version(min)

        if (stage == PreRelease && incMin && vmin.qualifier == "") {
          vmin = new Version(vmin.major, vmin.minor, vmin.micro, "", VersionStage.PreRelease)
        }

        if (!incMax && vmax.qualifier == "") {
          vmax = new Version(vmax.major, vmax.minor, vmax.micro, "", VersionStage.PreRelease)
        }

        new VersionRange(incMin, vmin, vmax, incMax)
      }
    }
    catch {
      case e: NumberFormatException => throw new IllegalArgumentException("Invalid version range " + str)
    }
  }
}

case class VersionRange(inclusiveMin: Boolean, minimum: Version, maximum: Version, inclusiveMax: Boolean) extends (Version => Boolean) {
  require(minimum <= maximum, "Minimum cannot be less than maximum")

  def apply(version: Version): Boolean = {
    def min = if (inclusiveMin) version >= minimum else version > minimum
    def max = if (inclusiveMax) version <= maximum else version < maximum
    min && max
  }

  // TODO add toLDAP test
  def toLDAP(key: String = "version"): LDAPExpr = {
    import LDAPExpr._
    if (inclusiveMax && inclusiveMin && maximum == Version.Infinite) {
      expr(key, `>=`, minimum.toString)
    }
    else {
      and(expr(key, if (inclusiveMin) `>=` else `>`, minimum.toString), expr(key, if (inclusiveMax) `<=` else `<`, maximum.toString))
    }
  }

  override def toString() = {
    if (inclusiveMax && inclusiveMin && maximum == Version.Infinite) {
      minimum.toString
    }
    else {
      (if (inclusiveMin) "[" else "(") + minimum + "," + maximum + (if (inclusiveMax) "]" else ")")
    }
  }
}
