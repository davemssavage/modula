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

package org.chronologicalthought.modula.osgi

import OSGiHeader._

import collection.mutable.{HashMap, ArrayBuffer}
import org.chronologicalthought.modula.LDAPExpr._
import org.chronologicalthought.modula._
import java.lang.{IllegalArgumentException, IllegalStateException}
import Constants.Directives._

/**
 * @author David Savage
 */

object OSGiHeader {
  implicit def stringToOSGiHeader(str: String) = new OSGiHeader(str)

  private def splitWithQuotes(string: String, delim: Char) = {
    val split = new ArrayBuffer[String]
    var quote = false
    val buf = new StringBuffer(string.length)

    for (i <- 0 until string.length) {
      val char = string.charAt(i)
      char match {
        case '"' => quote = !quote
        case `delim` => {
          if (!quote) {
            split += buf.toString.trim
            buf.setLength(0)
          }
          else {
            buf.append(delim)
          }
        }
        case c => {
          buf.append(c)
        }
      }
    }

    if (quote) {
      throw new IllegalArgumentException("""Unclosed " in """ + string)
    }
    else {
      if (buf.toString.length > 0) {
        split += buf.toString.trim
      }

      split.toList
    }
  }

  private class NameToMap(val name: String, val attributes: Map[String, String], val directives: Map[String, String]) {
    override def toString = name + "->" + attributes + "->" + directives
  }

  // attr=value
  // dir:=val
  private def splitParts(header: String): NameToMap = {
    val parts = splitWithQuotes(header, ';')
    val name = parts.head
    val rest = parts.tail
    val attrs = new HashMap[String, String]()
    val directives = new HashMap[String, String]()

    // TODO add test for unquote behaviour
    def unquote(str: String) = {
      if (str.startsWith("\"")) {
        str.substring(1, str.length() - 1)
      }
      else {
        str
      }
    }

    for (kv <- rest) {
      val split = kv.split("""\s*=\s*""")
      // TODO add context information to aid debug
      if (split.length != 2) throw new IllegalArgumentException("Expected key=value or key:=value, got " + kv);

      if (split(0).endsWith(":")) {
        val k = split(0).substring(0, split(0).length() - 1)
        val v = unquote(split(1))
        directives += k -> v
      }
      else {
        val k = split(0)
        val v = unquote(split(1))
        attrs += k -> v
      }
    }

    new NameToMap(name, attrs.toMap, directives.toMap)
  }

}

class OSGiHeader(header: String) {

  def buildPackageExports(part: Part = PrimordialPart): List[Capability] = {
    val ns = Constants.PackageNamespace
    val caps = for (n2p <- mapHeader) yield {
      // ensure ns overwrites root attrs
      // TODO log warning throw error if header invalid?
      val m = n2p.attributes ++ Map(ns -> n2p.name)
      part.newCapability(ns, m, n2p.directives)
    }
    caps.toList
  }

  def buildPackageImports(part: Part): List[Requirement] = {
    val ns = Constants.PackageNamespace
    val reqs = for (n2p <- mapHeader) yield {
      // ensure ns overwrites root attrs
      // TODO log warning throw error if header invalid?
      // TODO map version to attributes/filter
      // Map(version -> [1.3,2))
      val m = n2p.attributes ++ Map(ns -> n2p.name)
      var filter: LDAPExpr = expr(ns, `=`, n2p.name)
      n2p.attributes.get("version") match {
        case Some(str) => {
          filter = and(filter, VersionRange(str.toString).toLDAP())
        }
        case None =>
      }
      var d: Map[String, Any] = n2p.directives.map(entry => {
        val key = entry._1
        val value = entry._2
        // TODO extract OSGi constants
        if (key == "resolution") {
          ResolutionPolicy -> {
            if ("mandatory" == value) {
              ResolutionMandatory
            }
            else if ("optional" == value) {
              ResolutionOptional
            }
            else if ("dynamic" == value) {
              ResolutionDynamic
            }
            else {
              throw new IllegalStateException("Invalid resolution directive: " + value)
            }
          }
        }
        else {
          entry
        }
      })

      d ++= Map(RequirementFilter -> filter)
      part.newRequirement(ns, m, d)
    }
    reqs.toList
  }

  def buildRequiredBundles(part: Part): List[Requirement] = {
    val ns = Constants.ModuleNamespace
    val reqs = for (i <- splitHeader) yield part.newRequirement(ns, Map(ns -> i), Map.empty)
    reqs.toList
  }

  def buildFragmentHost(part: Part): Requirement = {
    val ns = Constants.ModuleNamespace
    val n2p = splitParts(header)
    val fragment = n2p.name
    part.newRequirement(ns, Map(ns -> fragment), Map(ExtensionExtends -> ns, RequirementFilter -> expr(ns, `=`, fragment)))
  }

  private def mapHeader = {
    splitHeader.map(splitParts(_))
  }

  private def splitHeader = splitWithQuotes(header, ',')
}
