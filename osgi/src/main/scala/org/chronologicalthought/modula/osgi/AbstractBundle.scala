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
import java.util.Enumeration
import java.util.Dictionary
import java.security.cert.X509Certificate
import java.io.File
import org.osgi.framework._
import org.chronologicalthought.modula.{Module, ModuleState}
import ModuleState._
import collection.JavaConversions
import java.util.regex.Pattern
import collection.JavaConversions._
import org.chronologicalthought.modula.{Failure, Empty, Full}
import concurrent.Await
import concurrent.duration.Duration


/**
 * @author David Savage
 */

abstract class AbstractBundle(config: Map[String, String], module: Module) extends Bundle {

  private[osgi] val ctx = new BundleContextImpl(this, module.context)

  // abstract methods...
  //  def getSymbolicName
  //
  //  def adapt[A](clazz: Class[A]): A
  //
  //  def update(input: InputStream)
  //
  //  def getLocation
  //
  //  def getLastModified
  //
  //  def uninstall()
  //
  def getBundleId() = module.id

  def loadClass(name: String) = module.loadClass(name) match {
    case Full(c) => c
    case Empty => throw new ClassNotFoundException("Unknown class %s".format(name))
    case f: Failure => throw f.exception.openOr(throw new IllegalStateException("Failed to load %s".format(name)))
  }

  def getVersion() = Version.parseVersion(module.version.toString)

  def getState(): Int = {
    module.state match {
      case Uninstalled => Bundle.UNINSTALLED
      case Installed => Bundle.INSTALLED
      case Resolved => Bundle.RESOLVED
      case Started => Bundle.ACTIVE
      case Starting => Bundle.STARTING
      case Stopping => Bundle.STOPPING
    }
  }

  def start(): Unit = {
    start(0)
  }

  def stop(): Unit = {
    stop(0)
  }

  def start(options: Int) {
    // TODO handle framework start options
    Await.result(module.start(), Duration.Inf)
  }

  def stop(options: Int) {
    // TODO handle framework stop options
    Await.result(module.stop(), Duration.Inf)
  }

  def update() = {
    update(null)
  }

  def getBundleContext = ctx

  def getRegisteredServices(): Array[ServiceReference[_]] = {
    // TODO implement bundle getRegisteredServices
    throw new IllegalStateException("Not yet implemented")
  }

  def getServicesInUse(): Array[ServiceReference[_]] = {
    // TODO implement bundle getServicesInUse
    throw new IllegalStateException("Not yet implemented")
  }

  def hasPermission(permission: AnyRef): Boolean = {
    // TODO implement bundle hasPermission
    false
  }

  def getResource(name: String): URL = {
    val relative = if (name.startsWith("/")) name else "/" + name
    def check(p: String) = {
      p == relative
    }
    module.resources(check(_), true, false) match {
      case head :: _ => head
      case _ => null
    }
  }

  def getHeaders(): Dictionary[String, String] = {
    collection.mutable.HashMap() ++ module.attributes.map(t => {
      val key = t._1
      val value = if (t._2 == null) null else t._2.toString
      key -> value
    })
  }

  def getHeaders(locale: String): Dictionary[String, String] = {
    // TODO implement bundle getHeaders(locale)
    throw new IllegalStateException("Not yet implemented")
  }

  def getResources(name: String): Enumeration[URL] = {
    val result = module.resources(_ == name, true, false)
    JavaConversions.asJavaEnumeration(result.toIterator)
  }

  /**
   * Returns a URL to the entry at the specified path in this bundle. This bundle's class loader is not used to search
   * for the entry. Only the contents of this bundle are searched for the entry.
   *
   * The specified path is always relative to the root of this bundle and may begin with "/". A path value of "/"
   * indicates the root of this bundle.
   *
   * Note: Jar and zip files are not required to include directory entries. URLs to directory entries will not be
   * returned if the bundle contents do not contain directory entries.
   *
   * @param path - The path name of the entry.
   *
   * @return A URL to the entry, or null if no entry could be found or if the caller does not have the appropriate
   * AdminPermission[this,RESOURCE] and the Java Runtime Environment supports permissions.
   *
   * @throws java.lang.IllegalStateException - If this bundle has been uninstalled.
   */
  def getEntry(path: String): URL = {
    val relative = if (path.startsWith("/")) path else "/" + path
    def check(p: String) = {
      p == relative
    }
    module.resources(check(_), false, true) match {
      case head :: _ => head
      case _ => null
    }
  }

  def getSignerCertificates(signersType: Int): java.util.Map[X509Certificate, java.util.List[X509Certificate]] = {
    // TODO implement bundle getSignerCertificates
    throw new IllegalStateException("Not yet implemented")
  }

  def getDataFile(fileName: String): File = {
    // TODO assert fileName not attempting to break out does that matter?
    val root = BundlePathUtils.bundleRoot(config, module.id)
    new File(root, "storage" + File.separator + fileName)
  }

  def compareTo(bundle: Bundle): Int = {
    val thisID = getBundleId
    val thatID = bundle.getBundleId

    thisID.compareTo(thatID)
  }

  /**
   * Returns an Enumeration of all the paths (String objects) to the entries within this bundle whose longest
   * sub-path matches the specified path.
   *
   * This bundle's class loader is not used to search for entries. Only the contents of this bundle are searched.
   *
   * The specified path is always relative to the root of this bundle and may begin with a "/".
   * A path value of "/" indicates the root of this bundle.
   *
   * Returned paths indicating subdirectory paths end with a "/". The returned paths are all relative to the root of
   * this bundle and must not begin with "/".
   *
   * Note: Jar and zip files are not required to include directory entries. Paths to directory entries will not be
   * returned if the bundle contents do not contain directory entries.
   */
  def getEntryPaths(path: String): Enumeration[String] = {
    val result = module.resources(filterFiles(_, path, "*", true), false, false)
    JavaConversions.asJavaEnumeration(result.map(_.toString).toIterator)
  }

  def findEntries(path: String, filePattern: String, recurse: Boolean) = {
    val result = module.resources(filterFiles(_, path, filePattern, recurse), true, false)
    JavaConversions.asJavaEnumeration(result.toIterator)
  }

  private def filterFiles(file: String, path: String, filePattern: String, recurse: Boolean): Boolean = {
    def isInPath = {
      val index = file.lastIndexOf('/')
      val dir = "/" + (if (index == -1 || index == 0) "" else file.substring(1, index))
      if (recurse)
        dir.startsWith(path)
      else
        dir == path
    }

    def glob(str: String) = {
      str.replace(".", "\\.").replace("*", ".*")
    }

    def isFile = {
      val pattern = Pattern.compile(glob(filePattern))
      pattern.matcher(file).matches
    }

    isInPath && isFile
  }


}
