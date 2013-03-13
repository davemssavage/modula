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

package org.chronologicalthought.modula.tools

import java.io._
import org.osgi.framework.Constants._
import com.google.common.io.ByteStreams.copy

import collection.immutable.SortedSet
import java.util.jar._
import java.util.zip.{ZipEntry, ZipOutputStream}
import collection.mutable.{HashMap, HashSet}
import org.osgi.framework.{BundleActivator, Version}
import java.lang.StringBuilder

object BundleBuilder {
  def newTestBundle(): BundleBuilder = {
    // TODO add assert=true constructor
    new BundleBuilder()
  }

  def newBundle(symbolicName: String): BundleBuilder = {
    new BundleBuilder().symbolicName(symbolicName);
  }
}

/**
 * @author David Savage
 */

class BundleBuilder private() {
  private var bsn: Option[String] = None
  private var version: Option[Version] = None

  private var host: Option[String] = None

  private var activator: Option[String] = None

  private var imports = SortedSet[String]()
  private var required = SortedSet[String]()
  private var exports = SortedSet[String]()
  private var resources = HashMap[String, File]()

  def symbolicName(name: String): BundleBuilder = {
    require(name != null)
    val t = name.trim
    require(t.length > 0)
    bsn = Some(t)
    this
  }

  def version(v: String): BundleBuilder = {
    version = Some(Version.parseVersion(v))
    this
  }

  def fragmentHost(bundleSymbolicName: String): BundleBuilder = {
    this.host = Some(bundleSymbolicName)
    this
  }

  def fragmentHost(bundleSymbolicName: String, version: String): BundleBuilder = {
    // TODO validate version range...
    this.host = Some(bundleSymbolicName + ";" + BUNDLE_VERSION_ATTRIBUTE + "=" + version)
    this
  }

  def requireBundle(bundleSymbolicName: String): BundleBuilder = {
    required += bundleSymbolicName
    this
  }

  def requireBundle(bundleSymbolicName: String, directives: Map[String, String]): BundleBuilder = {
    val buf = new StringBuilder
    buf.append(bundleSymbolicName)
    appendMap(buf, directives, ":=")
    required += buf.toString
    this
  }

  def importPackage(p: Package): BundleBuilder = {
    // TODO infer version from current classpath?
    importPackage(p.getName)
  }

  def importPackage(name: String): BundleBuilder = {
    imports += name
    this
  }

  def importPackage(name: String, version: String): BundleBuilder = {
    importPackage(name, Version.parseVersion(version))
  }

  def importPackage(name: String, version: Version): BundleBuilder = {
    imports += name + ";version=" + version.toString
    this
  }

  def exportPackage(p: Package): BundleBuilder = {
    // TODO infer version from current classpath?
    exportPackage(p.getName)
  }

  def exportPackage(name: String): BundleBuilder = {
    exports += name
    this
  }

  def exportPackage(name: String, version: Version): BundleBuilder = {
    exports += name + ";version=" + version.toString
    this
  }

  def exportPackage(name: String, attributes: Map[String, String], directives: Map[String, String]): BundleBuilder = {
    val buf = new StringBuilder
    buf.append(name)
    appendMap(buf, attributes, "=")
    appendMap(buf, directives, ":=")

    exports += buf.toString
    this
  }

  def addResource(path: String, content: InputStream): BundleBuilder = {
    val tmp = File.createTempFile("tmp", ".res")
    tmp.deleteOnExit

    val out = new FileOutputStream(tmp)

    try {
      copy(content, out)
    }
    finally {
      out.close
    }

    resources.synchronized {
      resources.get(path) match {
        case Some(f) => {
          f.delete
        }
        case None => // fine
      }

      resources += path -> tmp
    }

    this
  }

  def addClass(clazz: Class[_]): BundleBuilder = {
    val location = clazz.getProtectionDomain.getCodeSource.getLocation
    if ("file" == location.getProtocol) {
      val file = location.getFile
      if (file.endsWith(File.separator)) {
        val classPath = clazz.getName.replace('.', File.separatorChar) + ".class"
        val classFile = new File(file, classPath)
        addResource(classPath, new FileInputStream(classFile))
      }
      else {
        // TODO load from jars etc
        throw new UnsupportedOperationException("Failed to load from " + location)
      }
    }
    else {
      // TODO load from jars etc
      throw new UnsupportedOperationException("Failed to load from " + location)
    }

    this
  }

  def activator(className: String): BundleBuilder = {
    activator = Some(className)
    this
  }

  def activator(clazz: Class[_ <: BundleActivator]): BundleBuilder = {
    activator(clazz.getName)
    addClass(clazz)
    this
  }

  def addResource(path: String, file: File): BundleBuilder = {
    val in = new FileInputStream(file)
    try {
      addResource(path, in)
    }
    finally {
      in.close()
    }
    this
  }

  def addResource(path: String, content: Array[Byte]): BundleBuilder = {
    addResource(path, new ByteArrayInputStream(content))
    this
  }

  def addResource(path: String, content: String): BundleBuilder = {
    addResource(path, new ByteArrayInputStream(content.getBytes("UTF-8")))
    this
  }

  def buildManifest: Manifest = {
    val mf = new Manifest
    val main = mf.getMainAttributes

    main.put(Attributes.Name.MANIFEST_VERSION, "1.0");
    main.putValue(BUNDLE_MANIFESTVERSION, "2");
    bsn match {
      case Some(bsn) => main.putValue(BUNDLE_SYMBOLICNAME, bsn)
      case None => // TODO support strict mode that throws exception
    }

    version match {
      case Some(v) => main.putValue(BUNDLE_VERSION, v.toString)
      case None => // fine
    }

    activator match {
      case Some(clazz) => main.putValue(BUNDLE_ACTIVATOR, clazz)
      case None => // fine
    }

    host match {
      case Some(host) => main.putValue(FRAGMENT_HOST, host)
      case None => // fine
    }

    if (!imports.isEmpty) {
      main.putValue(IMPORT_PACKAGE, imports.mkString(","))
    }

    if (!required.isEmpty) {
      main.putValue(REQUIRE_BUNDLE, required.mkString(","))
    }

    if (!exports.isEmpty) {
      main.putValue(EXPORT_PACKAGE, exports.mkString(","))
    }

    mf
  }

  def mkTmpJar(): JarFile = {
    new JarFile(mkTmpFile)
  }

  def mkTmpFile(): File = {
    val f = File.createTempFile("tmp", ".jar")
    f.deleteOnExit

    val out = new FileOutputStream(f)

    try {
      write(out)
    }
    finally {
      out.close
    }

    f
  }

  def write(out: OutputStream) {
    val zip = new ZipOutputStream(out)
    zip.setComment("Created by BundleBuilder")

    val directories = new HashSet[String]()

    def createDirectories(name: String) {
      val index = name.lastIndexOf('/');
      if (index > 0) {
        val path = name.substring(0, index);
        if (directories.contains(path))
          return;

        createDirectories(path);

        val ze = new ZipEntry(path + '/');
        zip.putNextEntry(ze);
        zip.closeEntry();
        directories.add(path);
      }
    }

    def createEntry(path: String)(f: => Unit) {
      createDirectories(path)
      val ze = new ZipEntry(path)
      ze.setMethod(ZipEntry.DEFLATED)
      zip.putNextEntry(ze)
      f
      zip.closeEntry
    }

    createEntry("META-INF/MANIFEST.MF") {
      buildManifest.write(zip)
    }

    resources.synchronized {
      val snapshot = Map.empty ++ resources

      for ((path, tmp) <- snapshot) {
        var in = new FileInputStream(tmp)
        try {
          createEntry(path) {
            copy(in, zip)
          }
        } finally {
          in.close
        }
      }
    }

    zip.finish
  }

  private def failIf(f: => Boolean, msg: String) {
    if (f) throw new IllegalStateException(msg)
  }

  private def appendMap(buf: StringBuilder, map: Map[String, String], eq: String) {
    if (!map.isEmpty) {
      val key = map.head._1
      val value = map.head._2
      buf.append(";")
      buf.append(key)
      buf.append(eq)
      buf.append(value)
      appendMap(buf, map.tail, eq)
    }
  }


}
