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

import org.osgi.framework.Constants._
import OSGiHeader._
import org.chronologicalthought.modula._
import annotation.tailrec
import java.io._
import org.osgi.framework.{BundleException, BundleActivator}
import java.util.zip.ZipInputStream
import collection.JavaConversions
import java.util.jar.{Attributes, Manifest}
import java.net.{MalformedURLException, URLClassLoader, URL}
import java.lang.{IllegalStateException, ClassNotFoundException, String}
import Constants.Directives._
import org.chronologicalthought.modula.{Empty, Failure, Full, Box}
import concurrent._
import ExecutionContext.Implicits.global

/**
 * @author David Savage
 */
class OSGiModuleProvider(workingDir: File, location: String, input: InputStream) extends ModuleProvider {
  private var ctx: BundleContextImpl = _
  private var activator: Option[BundleActivator] = None
  private var classLoader: Option[ClassLoader] = None
  private var resourceLoader: Option[ResourceLoader] = None
  private var workingFiles: List[URL] = _
  private lazy val rc = new RequirementCapabilityHelper(this)

  private[osgi] def context_=(ctx: BundleContextImpl) {
    this.ctx = ctx
  }

  // TODO should what should be the name of an unnamed bundle - check osgi spec
  def name = attributes.getOrElse(BUNDLE_SYMBOLICNAME, attributes.getOrElse(BUNDLE_NAME, ""))

  //def name = attributes.getOrElse(BUNDLE_SYMBOLICNAME, throw new IllegalStateException("Missing bundle symbolic name"))

  def version = Version(attributes.getOrElse(BUNDLE_VERSION, "0.0.0"))

  lazy val attributes = loadBundleHeaders()

  lazy val capabilities: List[Capability] = {
    rc.bundleCapability :: rc.exportedPackages
  }

  lazy val requirements: List[Requirement] = {
    rc.importedPackages ::: rc.requiredBundles ::: rc.fragmentHost
  }

  def init(parent: ResourceLoader) {
    resourceLoader = Some(parent)
    workingFiles = unpackWorkingFiles()
    classLoader = Some(new URLClassLoader(workingFiles.toArray, parent))
    assertValid()
  }

  def destroy() {
    classLoader = None
  }

  def start(): Future[ModuleLike] = {
    Future {
      try {
        activator(true) match {
          case Some(ba) => ba.start(ctx)
          case None =>
        }
      }
      catch
      {
        case e: NoClassDefFoundError => {
          throw e.getCause
        }
      }
      OSGiModuleProvider.this
    }
  }

  def stop(): Future[ModuleLike] = {
    Future {
      activator(false) match {
        case Some(ba) => ba.stop(ctx)
        case None =>
      }
      OSGiModuleProvider.this
    }
  }

  def loadClass(name: String): Box[Class[_]] = {
    // TODO require bundle support may mean classloader cannot be generic
    // use builder pattern for maps to make it easy for
    // other modules to use "correct" package wiring approach
    classLoader match {
      case Some(c) => {
        try {
          Full(c.loadClass(name))
        }
        catch {
          case e: NoClassDefFoundError => {
            Failure("Failed to load %s".format(name), Full(new ClassNotFoundException(name)), Empty)
          }
          case e: ClassNotFoundException => {
            Failure("Failed to load %s".format(name), Full(e), Empty)
          }
        }
      }
      case None => {
        val msg = "Bundle %s stopped".format(location)
        Failure(msg, Full(new IllegalStateException(msg)), Empty)
      }
    }
  }

  def resources(filter: (String) => Boolean, resolve: Boolean, local: Boolean): Traversable[URL] = {
    def localResources = {
      for (
        url <- workingFiles;
        file <- findEntries(url, filter)
      ) yield file
    }

    if (local) {
      localResources
    }
    else {
      def otherResources = {
        resourceLoader match {
          case Some(l) => l.resources(filter, resolve)
          case None => Nil
        }
      }
      localResources ++: otherResources
    }
  }

  private def findEntries(url: URL, filter: (String) => Boolean): Seq[URL] = {
    // TODO exception can cause stream to not close!! = how to do a finally in a seq?
    val in = new ZipInputStream(url.openStream)
    val stream = Stream.continually(in.getNextEntry)
    val seq = stream.view.takeWhile(entry => {
      if (entry == null) {
        in.close()
        false
      }
      else {
        true
      }
    }).map("/" + _.getName).filter(filter(_)).map(name => new URL("jar:" + url + "!" + name)).force
    if (filter("/")) {
      new URL("jar:" + url + "!/") +: seq
    }
    else {
      seq
    }
  }

  private def assertValid() {
    // TODO this has no effect bsn is no longer checked as mandatory
    try {
      name
    }
    catch {
      case e: Exception => throw new BundleException("Invalid bundle ", e)
    }
  }

  private def activator(start: Boolean): Option[BundleActivator] = {
    def loadActivator(s: String) = {
      val clazz = loadClass(s).openOr(throw new BundleException("Failed to load activator: " + s))
      clazz.newInstance().asInstanceOf[BundleActivator]
    }

    attributes.get(BUNDLE_ACTIVATOR) match {
      case Some(s: String) => {
        if (start) {
          val ba = loadActivator(s)
          synchronized {
            activator = Option(ba)
            return activator
          }
        } else {
          synchronized {
            return activator
          }
        }
      }
      case None => None
    }
  }

  private def unpackWorkingFiles() = {
    def safeMkdir(file: File) = {
      file.isDirectory || file.mkdirs
    }

    def safeFileName(location: String): String = {
      var l = try {
        new URL(location).getFile
      }
      catch {
        case e: MalformedURLException => {
          location
        }
      }
      l = l.replace('/', File.separatorChar)
      l = l.replace('!', File.separatorChar)
      l = l.replace(':', File.separatorChar)
      l
    }

    def download(input: InputStream, file: File): URL = {
      try {
        if (!safeMkdir(file.getParentFile)) throw new BundleException("Failed to make tmp file")

        val out = new FileOutputStream(file)

        // TODO make buffer size configurable
        val buf = new Array[Byte](1024 * 1024)

        @tailrec
        def stream(in: InputStream, out: OutputStream) {
          in.read(buf) match {
            case -1 =>
            case n => {
              out.write(buf, 0, n)
              stream(in, out)
            }
          }
        }

        stream(input, out)
        input.close()
        out.close()

        file.toURI.toURL
      } catch {
        case e: IOException => throw new BundleException("Failed to download " + location, e)
      }
    }

    if (!safeMkdir(workingDir)) throw new BundleException("Failed to create working directory")

    val file = new File(workingDir, safeFileName(location))
    val cached = download(input, file)

    List(cached)
  }

  private def loadBundleHeaders(): Map[String, String] = {
    val manifest = findEntries(workingFiles.head, (name) => name == "/META-INF/MANIFEST.MF")
    if (manifest.isEmpty) {
      throw new BundleException("Missing manifest entry")
    }

    val mf = new Manifest()
    val in = manifest.head.openStream
    try {
      mf.read(in)

      var headers = Map.empty[String, String]
      for (entry <- JavaConversions.asScalaSet(mf.getMainAttributes.entrySet)) {
        val name = entry.getKey.asInstanceOf[Attributes.Name]
        headers += name.toString -> entry.getValue.asInstanceOf[String]
      }

      headers
    }
    finally {
      in.close()
    }
  }

  override def toString = "OSGiModuleProvider[" + name + ":" + version + "]"
}

class RequirementCapabilityHelper(provider: OSGiModuleProvider) {
  def bundleCapability: Capability = {
    val ns = Constants.ModuleNamespace
    provider.newCapability(ns, Map(ns -> provider.name), Map(ExtensionExtensible -> "true"))
  }

  def exportedPackages: List[Capability] = {
    provider.attributes.get(EXPORT_PACKAGE) match {
      case Some(exports) => {
        exports.buildPackageExports(provider)
      }
      case None => Nil
    }
  }

  def importedPackages: List[Requirement] = {
    provider.attributes.get(IMPORT_PACKAGE) match {
      case Some(imports) => {
        imports.buildPackageImports(provider)
      }
      case None => Nil
    }
  }

  def requiredBundles: List[Requirement] = {
    provider.attributes.get(REQUIRE_BUNDLE) match {
      case Some(bundles) => {
        bundles.buildRequiredBundles(provider)
      }
      case None => Nil
    }
  }

  def fragmentHost: List[Requirement] = {
    provider.attributes.get(FRAGMENT_HOST) match {
      case Some(fragment) => {
        // TODO this should be an optional requirement ??
        fragment.buildFragmentHost(provider) :: Nil
      }
      case None => Nil
    }
  }
}
