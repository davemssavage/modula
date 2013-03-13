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

import org.chronologicalthought.modula._

import util.DynamicVariable
import java.net.URL
import ModuleProviderClassLoader._
import java.lang.ClassNotFoundException

object ModuleProviderClassLoader {
  private val resourcesCycleCheck = new DynamicVariable[Set[Module]](Set.empty)
}

/**
 * @author David Savage
 */

class ModuleProviderClassLoader(module: ResolvableModule, parent: ClassLoader) extends ResourceLoader(parent) {

  private val classLoaderCycleCheck = new DynamicVariable[Set[String]](Set.empty)

  def resources(filter: (String) => Boolean, wire: Boolean): Traversable[URL] = {
    val current = resourcesCycleCheck.value
    if (current.contains(module)) {
      Nil
    }
    else {
      resourcesCycleCheck.withValue(current + module) {
        val local = false
        val modules = module.resolveModules(wire)
        modules.dependents.flatMap(_.resources(filter, wire, local)) ++: modules.extensions.flatMap(_.resources(filter, wire, local))
      }
    }
  }

  override def loadClass(name: String, resolve: Boolean): Class[_] = {
    var clazz = findLoadedClass(name)

    if (clazz == null) {
      def toPackageName = {
        val dot = name.lastIndexOf('.')
        if (dot == -1) "" else name.substring(0, dot)
      }

      val p = toPackageName

      if (p.length == 0) {
        // TODO think no module can export default package?
        // throw cnfe to get local module to delegate
        throw new ClassNotFoundException(name)
      }
      else {
        // TODO need config support here
        val pdot = p + "."
        if (pdot.startsWith("java.") || pdot.startsWith("sun.")) {
          clazz = parent.loadClass(name)
        }
        else {
          val current = classLoaderCycleCheck.value
          if (!current.contains(name)) {
            clazz = classLoaderCycleCheck.withValue(current + name) {
              searchModuleSpace(p, name).openOr(throw new ClassNotFoundException(name))
            }
            if (clazz == null) {
              throw new ClassNotFoundException(name)
            }
            else if (resolve) {
              resolveClass(clazz)
            }
          }
        }
      }
    }

    clazz
  }

  private def searchModuleSpace[T](pkg: String, name: String): Box[Class[T]] = {
    val modules = module.resolveModules(wire = true)

    def exportsPackage(capability: Capability): Boolean = {
      import Constants.PackageNamespace
      capability.namespace == PackageNamespace && capability.attributes(PackageNamespace) == pkg
    }

    // TODO handle split packages by walking vs assuming first?
    // capability should have directive to state whether can merge split packages
    // handle similar to fragements?
    modules.dependents.find(_.capabilities.exists(exportsPackage)) match {
      case Some(m) => m.loadClass(name).asInstanceOf[Box[Class[T]]]
      case None => {
        // ok dependencies didn't find it try extensions
        val start: Box[Class[_]] = Empty

        def firstLoaded(found: Box[Class[_]], nextModule: ModuleLike) = found.or(nextModule.loadClass(name))

        val clazz = modules.extensions.foldLeft(start)(firstLoaded)

        clazz.map(_.asInstanceOf[Class[T]])
      }
    }
  }
}
