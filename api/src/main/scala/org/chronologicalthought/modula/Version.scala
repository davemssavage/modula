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

import collection.mutable.WeakHashMap
import org.chronologicalthought.modula.VersionStage._
import java.lang.IllegalArgumentException

/**
 * @author David Savage
 */

object Version {
  private val pattern = """(\d+(\.\d+(\.\d+([\.-].*)?)?)?)*""".r

  private val cache = new WeakHashMap[String, Version]

  val Empty = Version(0, 0, 0, "", VersionStage.PreRelease).intern

  val Infinite = Version(Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE, null, VersionStage.Release)

  def apply(str: String): Version = {
    val s = str.trim
    cache.getOrElseUpdate(s.intern, parseVersion(s))
  }

  private def parseVersion(str: String): Version = {
    def extractVersionParts: Option[List[String]] = {
      // pattern.unapplySeq generates list of the form:
      // Some(List(1.2.3-SNAPSHOT, .2.3-SNAPSHOT, .3-SNAPSHOT, -SNAPSHOT))
      // this function unpacks this to remove redundant information
      // i.e. the above becomes
      // Some(List(1, .2, .3, -SNAPSHOT))
      def unpack(list: List[String]): List[String] = {
        if (list.isEmpty) {
          list
        }
        else {
          if (list.tail.isEmpty) {
            list
          }
          else {
            list.head.reverse.substring(list.tail.head.length()).reverse :: unpack(list.tail)
          }
        }
      }
      // filter null as versions of the form "1" generate a list containing
      // Some(List(1, null, null, null))
      pattern.unapplySeq(str).map(list => unpack(list.filter(_ != null)))
    }

    def buildVersion(parts: List[String]) = {
      def toInt(str: String): Int = {
        str match {
          case "" => 0
          case s => s.toInt
        }
      }

      var m1 = 0
      var m2 = 0
      var m3 = 0
      var q = ""
      var s = Release

      parts match {
        case Nil => {
          s = VersionStage.PreRelease
        }
        case major :: Nil => {
          m1 = toInt(major)
        }
        case major :: minor :: Nil => {
          m1 = toInt(major)
          m2 = toInt(minor.substring(1))
        }
        case major :: minor :: micro :: Nil => {
          m1 = toInt(major)
          m2 = toInt(minor.substring(1))
          m3 = toInt(micro.substring(1))
        }
        case major :: minor :: micro :: qualifier :: Nil => {
          m1 = toInt(major)
          m2 = toInt(minor.substring(1))
          m3 = toInt(micro.substring(1))
          s = if (qualifier.startsWith("."))
            Release
          else
            PreRelease

          q = qualifier.substring(1)
        }
        case _ => throw new IllegalArgumentException("Unexpected version match for " + str)
      }

      new Version(m1, m2, m3, q, s)
    }

    extractVersionParts.map(buildVersion).getOrElse(throw new IllegalArgumentException("Invalid version format " + str))
  }
}

final case class Version(major: Int, minor: Int, micro: Int, qualifier: String, stage: VersionStage = Release) extends Ordered[Version] {
  assert(major >= 0, "Major version must be greater than zero")
  assert(minor >= 0, "Minor version must be greater than zero")
  assert(micro >= 0, "Micro version must be greater than zero")

  private lazy val str = buildStr

  override def toString = str

  override def equals(other: Any): Boolean = {
    other match {
      case v: Version => {
        if (this eq v) true
        else if (v eq null) false
        else str eq v.str
      }
      case _ => false
    }
  }

  override val hashCode = {
    41 * str.hashCode
  }

  def intern = Version(str)

  private def buildStr: String = {
    def appendParts(v: String, buf: List[String]): List[String] = {
      def pushValue(value: String) = if (buf.isEmpty) Nil else value :: Nil
      def pushDot(r: List[String]) = if (buf.isEmpty || buf.headOption.exists(v => v == "." || v == "-")) r else r ::: "." :: Nil

      val next = v match {
        case "" => Nil
        case "0" => pushDot(pushValue("0"))
        case "." => pushValue(".")
        case "-" => pushValue("-")
        case other: String => pushDot(other :: Nil)
      }

      next ::: buf
    }

    val mj = major.toString
    val mn = minor.toString
    val mc = micro.toString
    val q = if (qualifier == null) "Infinity" else qualifier
    val s = if (stage == Release) "." else "-"

    val parts = mj :: mn :: mc :: s :: q :: Nil
    val start: List[String] = Nil

    parts.foldRight(start)(appendParts).mkString("")
  }

  def compare(that: Version) = {
    def compare[T](v1: Ordered[T], v2: T) = {
      val c = v1.compare(v2)
      if (c == 0) None else Some(c)
    }

    def compareQualifier[T](v1: String, v2: String) = {
      // null represents infinity
      if (v1 == null) {
        if (v2 == null) None else Some(1)
      }
      else {
        if (v2 == null) Some(-1) else compare(v1, v2)
      }
    }

    val result = compare(major, that.major)
      .orElse(compare(minor, that.minor))
      .orElse(compare(micro, that.micro))
      .orElse(compare(stage, that.stage))
      .orElse(compareQualifier(qualifier, that.qualifier))

    result.getOrElse(0)
  }
}
