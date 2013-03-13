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

import java.net.URL
import java.io._
import java.util.concurrent.atomic.AtomicLong
import collection.{immutable => i}
import collection.{mutable => m}
import org.chronologicalthought.modula.{ModuleContext, Module, ModuleProvider, ServiceRegistration}
import org.osgi.framework.{Constants, Bundle, BundleException}
import org.chronologicalthought.modula.Box

object BundlePathUtils {
  def bundleRoot(config: Map[String, String], id: Long): File = {
    val root = new File(config.get(Constants.FRAMEWORK_STORAGE) match {
      case Some(str) => str
      case None => "var" + File.separator + "bundles"
    })

    new File(root, id.toString)
  }
}

/**
 * @author David Savage
 */

class BundleFactoryImpl(config: Map[String, String], context: ModuleContext) extends BundleFactory {
  factory =>

  private val bundleCount = new AtomicLong
  private val locationToBundle = new m.HashMap[String, Bundle]
  private val locationToCache = new m.HashMap[String, File]

  private var idToBundle = new i.TreeMap[Long, Bundle]

  synchronized {
    // TODO listen for modules and map to ids/locations
    context.modules.foreach(t => {
      val id = t._1
      val module = t._2
      val bundle = new BundleWrapper(module)
      idToBundle += id -> bundle
      locationToBundle += bundle.getLocation -> bundle
    })
  }

  @throws(classOf[BundleException])
  def createBundle(location: String, input: InputStream): Bundle = {
    def newWorkingDir() = {
      val root = BundlePathUtils.bundleRoot(config, bundleCount.incrementAndGet)
      new File(root, "cache")
    }

    def register(provider: OSGiModuleProvider) = {
      val reg = context.register(provider, classOf[ModuleProvider])
      val found = context.findServices(classOf[Module]).filter(_.isProvidedBy(provider))
      if (found.isEmpty) {
        reg.unregister()
        throw new BundleException("Failed to locate installed module")
      }
      else {
        (reg, found.head)
      }
    }

    try {
      val in = if (input == null) new URL(location).openStream else input

      synchronized {
        locationToBundle.getOrElseUpdate(location, {
          val workingDir = newWorkingDir()

          locationToCache += location -> workingDir

          val provider = new OSGiModuleProvider(workingDir, location, in)

          val (reg, module) = register(provider)

          val bundle = new BundleImpl(location, reg, module)

          provider.context_=(bundle.ctx)

          idToBundle += module.id -> bundle

          bundle
        })
      }
    }
    catch {
      case e: IOException => throw new BundleException("Failed to install bundle", e)
    }
  }

  def deleteBundle(bundle: Bundle) {
    def deleteDir(dir: File) {
      dir.listFiles.foreach(f => {
        if (f.isDirectory) {
          deleteDir(f)
        }
        else {
          if (!f.delete) throw new BundleException("Failed to delete " + f)
        }
      })
      dir.delete
    }

    val loc = bundle.getLocation

    val workingDir = synchronized {
      locationToBundle.remove(loc)
      locationToCache.remove(loc)
    }

    workingDir match {
      case Some(cache) => deleteDir(cache)
      case None => // TODO log
    }
  }

  def getBundle(location: String): Box[Bundle] = {
    synchronized {
      locationToBundle.get(location)
    }
  }

  def getBundle(id: Long): Box[Bundle] = {
    synchronized {
      idToBundle.get(id)
    }
  }

  def getBundles(): List[Bundle] = {
    synchronized {
      idToBundle.values.toList
    }
  }

  class BundleImpl(location: String, registration: ServiceRegistration[OSGiModuleProvider], module: Module)
    extends AbstractBundle(config, module) {

    private var lastModified = System.currentTimeMillis

    def getSymbolicName = module.name

    def getLocation = location

    def getLastModified = synchronized(lastModified)

    def adapt[A](className: Class[A]) = {
      // TODO implement adapt
      throw new IllegalStateException("Not yet implemented")
    }

    def update(p1: InputStream) {
      synchronized {
        lastModified = System.currentTimeMillis
      }
      throw new IllegalStateException("Not yet implemented")
    }

    def uninstall() {
      registration.unregister()
      factory.deleteBundle(this)
    }
  }

  class BundleWrapper(module: Module) extends AbstractBundle(config, module) {
    // TODO module events for internal modifications?
    private val lastModified = System.currentTimeMillis

    lazy val getSymbolicName = if (module.id == 0) Constants.SYSTEM_BUNDLE_SYMBOLICNAME else module.name
    lazy val getLocation = if (module.id == 0) Constants.SYSTEM_BUNDLE_LOCATION else "mod:" + getSymbolicName

    def getLastModified = lastModified

    def adapt[A](className: Class[A]) = {
      // TODO implement adapt
      throw new IllegalStateException("Not yet implemented")
    }

    def update(p1: InputStream) {
      throw new BundleException("Synthetic bundles cannot be updated", BundleException.UNSUPPORTED_OPERATION)
    }

    def uninstall() {
      throw new BundleException("Synthetic bundles cannot be uninstalled", BundleException.UNSUPPORTED_OPERATION)
    }
  }

}
