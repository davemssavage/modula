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

import org.junit.Assert._
import org.junit.Test
import java.util.jar.JarFile
import org.osgi.framework.Constants

import BundleBuilder._
import io.Source

/**
 * @author David Savage
 */

class BundleBuilderTest {
  @Test
  def buildSimpleBundle() {
    val b = newBundle("foo")

    val jar = b.mkTmpJar

    assertManifestAttribute(jar, "foo", Constants.BUNDLE_SYMBOLICNAME)
  }

  @Test
  def buildImportBundle() {
    val b = newBundle("foo")

    b importPackage "foo"

    val jar = b.mkTmpJar

    assertManifestAttribute(jar, "foo", Constants.IMPORT_PACKAGE)
  }

  @Test
  def buildExportBundle() {
    val b = newBundle("foo")

    b exportPackage "foo"

    val jar = b.mkTmpJar

    assertManifestAttribute(jar, "foo", Constants.EXPORT_PACKAGE)
  }

  @Test
  def buildBundleWithResource() {
    val b = newBundle("foo")

    b addResource ("foo.txt", "wibble")

    val jar = b.mkTmpJar

    val e = jar.getJarEntry("foo.txt")

    assertNotNull(e)

    val src = Source.fromInputStream(jar.getInputStream(e))

    val lines = src.getLines

    assertFalse(lines.isEmpty)

    assertEquals("wibble", lines.next)

    assertTrue(lines.isEmpty)

    src.close
  }

  private def assertManifestAttribute(jar: JarFile, expected: String, attribute: String) {
    assertEquals(expected, jar.getManifest.getMainAttributes.getValue(attribute))
  }

}
