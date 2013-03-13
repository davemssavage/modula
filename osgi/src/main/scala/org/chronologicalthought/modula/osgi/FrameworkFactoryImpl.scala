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

import org.chronologicalthought.modula.{Capability, Constants => MConstants, PrimordialPart}
import org.chronologicalthought.modula.factory.GlobalFactory
import java.{util => ju}
import collection.JavaConversions._
import org.osgi.framework.launch.{Framework, FrameworkFactory}
import org.osgi.framework.{Bundle, Constants}
import OSGiHeader._
import java.lang.{IllegalStateException, String}
import org.osgi.util.tracker.ServiceTracker
import org.osgi.service.packageadmin.PackageAdmin
import org.osgi.service.startlevel.StartLevel

/**
 * @author David Savage
 */

class FrameworkFactoryImpl extends FrameworkFactory {
  def newFramework(config: ju.Map[String, String]): Framework = {
    val map = config.toMap
    val map2 = map.map {
      entry => {
        if (entry._1 == Constants.FRAMEWORK_BUNDLE_PARENT) {
          if (entry._2 == FrameworkFactoryImpl.NullParent) {
            GlobalFactory.ParentClassLoader -> new ClassLoader() {
              override def loadClass(name: String, resolve: Boolean) = {
                // TODO this needs to be correlated with the boot delegation in ModuleProviderClassLoader
                if (name.startsWith("java.") || name.startsWith("sun.")) {
                  ClassLoader.getSystemClassLoader.loadClass(name)
                }
                else {
                  throw new ClassNotFoundException(name)
                }
              }
            }
          }
          else {
            entry
          }
        }
        else if (entry._1 == Constants.FRAMEWORK_SYSTEMPACKAGES_EXTRA) {
          GlobalFactory.ExtraCapabilities -> entry._2.buildPackageExports()
        }
        else {
          entry
        }
      }
    }

    // TODO check system packages hasn't overriden this
    val extra = map2.get(GlobalFactory.ExtraCapabilities) match {
      case Some(list: List[Capability]) => list
      case Some(other) => throw new IllegalStateException("Invalid capabilities type " + other)
      case None => Nil
    }
    val caps = extra ::: frameworkPackages()
    val map3 = map2 ++ Map(GlobalFactory.ExtraCapabilities -> caps)
    new FrameworkImpl(map, GlobalFactory.newFramework(map3))
  }

  private def frameworkPackages(): List[Capability] = {
    def packageCapability(pckg: Package, version: String): Capability = {
      val ns = MConstants.PackageNamespace
      val m = Map(ns -> pckg.getName, "version" -> version)
      PrimordialPart.newCapability(ns, m, Map.empty)
    }

    // TODO this is unmanagable need profile approach
    packageCapability(classOf[Bundle].getPackage, "1.5.0") ::
      packageCapability(classOf[Framework].getPackage, "1.0.0") ::
      packageCapability(classOf[ServiceTracker[_, _]].getPackage, "1.4.0") ::
      packageCapability(classOf[PackageAdmin].getPackage, "1.2.0") ::
      packageCapability(classOf[StartLevel].getPackage, "1.1.0") ::
      Nil
  }
}

object FrameworkFactoryImpl {
  val NullParent = "null"
}
