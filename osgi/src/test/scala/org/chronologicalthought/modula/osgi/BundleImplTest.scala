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

import org.junit.Assert._
import collection.JavaConversions._
import io.Source
import org.chronologicalthought.modula.tools.BundleBuilder._
import java.net.URL
import org.osgi.framework._
import launch.Framework
import org.junit.{Ignore, Before, Test}
import java.lang.IllegalStateException
import org.chronologicalthought.modula.osgi.test.api.TestService
import org.chronologicalthought.modula.osgi.test.activator.TestActivator
import collection.JavaConversions
import util.Random

/**
 * @author David Savage
 */

class BundleImplTest {

  private var framework: Framework = _
  private var ctx: BundleContext = _


  @Before
  def setup() {
    framework = newFramework()
    ctx = framework.getBundleContext
  }

  @Test
  def getResource() {
    val bld = newBundle("foo")
    bld addResource ("foo.txt", "wibble")

    val f = bld.mkTmpFile()

    val b = framework.getBundleContext.installBundle(f.toURI.toURL.toExternalForm)

    val res = b.getResource("foo.txt")

    assertNotNull(res)

    assertContents(res, "wibble")
  }

  @Test
  def findEntriesNonRecursive() {
    def test(num: Int) {
      val bld = newBundle("foo" + num)

      bld addResource ("foo.txt", "wibble")
      bld addResource ("bar.txt", "wobble")

      val f = bld.mkTmpFile()

      val b = framework.getBundleContext.installBundle(f.toURI.toURL.toExternalForm)

      val res = b.findEntries("/", "*.txt", false)

      assertNotNull(res)

      assertTrue(res.hasMoreElements)

      assertContents(res.nextElement, "wibble", "wobble")

      assertTrue(res.hasMoreElements)

      assertContents(res.nextElement, "wibble", "wobble")

      assertFalse(res.hasMoreElements)
    }

    for (i <- 1 to 100) test(i)
  }

  @Test
  def findEntriesRecursive() {
    def test(num: Int) {
      val bld = newBundle("foo" + num)

      bld addResource ("foo.txt", "foo")
      bld addResource ("foo/foo.txt", "foo")
      bld addResource ("foo/foo/foo.txt", "foo")
      bld addResource ("bar.txt", "bar")
      bld addResource ("bar/bar.txt", "bar")
      bld addResource ("bar/bar/bar.txt", "bar")

      val f = bld.mkTmpFile()

      val b = framework.getBundleContext.installBundle(f.toURI.toURL.toExternalForm)

      def assertFound(dir: String, expected: Int, recurse: Boolean, check: String*) {
        val res = b.findEntries(dir, "*.txt", recurse)

        assertNotNull(res)

        var found = 0
        while (res.hasMoreElements) {
          found += 1
          assertContents(res.nextElement, check: _*)
        }
        assertEquals(expected, found)
      }

      assertFound("/", 6, true, "foo", "bar")
      assertFound("/foo", 1, false, "foo")
      assertFound("/bar", 1, false, "bar")
      assertFound("/foo", 2, true, "foo")
      assertFound("/bar", 2, true, "bar")
      assertFound("/foo/foo", 1, false, "foo")
      assertFound("/bar/bar", 1, false, "bar")
      assertFound("/foo/foo", 1, true, "foo")
      assertFound("/bar/bar", 1, true, "bar")
    }

    for (i <- 1 to 100) test(i)
  }

  @Test
  def testStartStopBundle() {
    val bld = newBundle("foo")

    bld activator (classOf[TestActivator])
    bld addClass (classOf[TestService])
    bld importPackage (classOf[BundleActivator].getPackage)

    val f = bld.mkTmpFile()

    val b = ctx.installBundle(f.toURI.toURL.toExternalForm)

    assertEquals(Bundle.INSTALLED, b.getState)

    b.start()

    assertEquals(Bundle.ACTIVE, b.getState)

    assertNotNull(ctx.getServiceReference(classOf[TestService].getName))

    b.stop()

    assertEquals(Bundle.RESOLVED, b.getState)

    assertNull(ctx.getServiceReference(classOf[TestService].getName))

    b.uninstall()

    assertEquals(Bundle.UNINSTALLED, b.getState)
  }

  @Test(expected = classOf[IllegalStateException])
  def loadClassFromStoppedBundle() {
    val f = newBundle("foo") addClass (classOf[TestService]) mkTmpFile()

    val b = ctx.installBundle(f.toURI.toURL.toExternalForm)
    b.uninstall()

    b.loadClass(classOf[TestService].getName)
  }

  @Test
  def testStartUninstallBundle() {
    val bld = newBundle("foo")

    bld activator (classOf[TestActivator])
    bld addClass (classOf[TestService])
    bld importPackage (classOf[BundleActivator].getPackage)

    val f = bld.mkTmpFile()

    val b = ctx.installBundle(f.toURI.toURL.toExternalForm)

    assertEquals(Bundle.INSTALLED, b.getState)

    b.start()

    assertEquals(Bundle.ACTIVE, b.getState)

    assertNotNull(ctx.getServiceReference(classOf[TestService].getName))

    b.uninstall()

    assertNull(ctx.getServiceReference(classOf[TestService].getName))

    assertEquals(Bundle.UNINSTALLED, b.getState)
  }

  @Test(expected = classOf[BundleException])
  @Ignore("pretty certain this is an invalid test for osgi - maybe depends on manifest version?")
  def testNoBSNHeader() {
    val bld = newTestBundle()
    ctx.installBundle(bld.mkTmpFile().toURI.toURL.toExternalForm)
  }

  @Test
  @Ignore("Test not yet implemented")
  def testNativeCode() {
    fail("Not testing native code")
  }

  @Test
  @Ignore("Test not yet implemented")
  def testBundleClasspath() {
    fail("Not yet testing this")
  }

  @Test(expected = classOf[BundleException])
  def testActivatorStartFailure() {
    val bld = newBundle("foo")

    bld activator (classOf[TestActivator])

    val f = bld.mkTmpFile()

    ctx.installBundle(f.toURI.toURL.toExternalForm).start()
  }

  @Test
  def importExport() {
    val api = newBundle("api")
    api addClass (classOf[TestService])
    api exportPackage (classOf[TestService].getPackage)
    api importPackage (classOf[ScalaObject].getPackage)

    val impl = newBundle("impl")
    impl activator (classOf[TestActivator])
    impl importPackage (classOf[BundleActivator].getPackage)
    impl importPackage (classOf[TestService].getPackage)
    impl importPackage (classOf[ScalaObject].getPackage)

    val a = api.mkTmpFile()
    val i = impl.mkTmpFile()

    ctx.installBundle(a.toURI.toURL.toExternalForm)
    val bi = ctx.installBundle(i.toURI.toURL.toExternalForm)

    bi.start()

    assertNotNull(ctx.getServiceReference(classOf[TestService].getName))

    bi.uninstall()

    assertNull(ctx.getServiceReference(classOf[TestService].getName))
  }

  @Test
  def attachFragment() {
    val host = newBundle("host")
    host activator (classOf[TestActivator])
    host importPackage (classOf[BundleActivator].getPackage)
    host importPackage (classOf[ScalaObject].getPackage)

    val fragment = newBundle("fragment")
    fragment fragmentHost ("host")
    fragment addClass (classOf[TestService])
    fragment importPackage (classOf[ScalaObject].getPackage)

    val h = host.mkTmpFile()
    val f = fragment.mkTmpFile()

    val bh = ctx.installBundle(h.toURI.toURL.toExternalForm)
    val bf = ctx.installBundle(f.toURI.toURL.toExternalForm)

    bh.start()

    assertNotNull(ctx.getServiceReference(classOf[TestService].getName))

    bh.uninstall()

    assertNull(ctx.getServiceReference(classOf[TestService].getName))
  }

  @Test
  def getEntryPaths() {
    val bundle = newBundle("host")
    bundle addClass (classOf[TestService])

    val bf = bundle.mkTmpFile()

    val b = ctx.installBundle(bf.toURI.toURL.toExternalForm)

    def assertEntries(assert: (Boolean) => Unit, path: String, expected: String) {
      val enum = b.getEntryPaths(path)
      var found: List[String] = Nil
      while (enum.hasMoreElements) {
        found = (enum.nextElement()) :: found
      }
      assert(found.exists(_.endsWith(expected)))
    }

    assertEntries(assertTrue, "/", "TestService.class")
    assertEntries(assertTrue, "/", "MANIFEST.MF")
    assertEntries(assertFalse, "/org", "MANIFEST.MF")
    assertEntries(assertTrue, "/org", "TestService.class")
  }

  @Test
  def fragmentLoadsClassFromHost() {
    val host = newBundle("host")
    host activator (classOf[TestActivator].getName)
    host addClass (classOf[TestService])
    host importPackage (classOf[BundleActivator].getPackage)
    host importPackage (classOf[ScalaObject].getPackage)

    val fragment = newBundle("fragment")
    fragment fragmentHost ("host")
    fragment addClass (classOf[TestActivator])
    fragment importPackage (classOf[ScalaObject].getPackage)

    val h = host.mkTmpFile()
    val f = fragment.mkTmpFile()

    val bh = ctx.installBundle(h.toURI.toURL.toExternalForm)
    val bf = ctx.installBundle(f.toURI.toURL.toExternalForm)

    bh.start()

    assertNotNull(ctx.getServiceReference(classOf[TestService].getName))

    bh.uninstall()

    assertNull(ctx.getServiceReference(classOf[TestService].getName))
  }

  @Test
  @Ignore("work in progress")
  def uninstallAttachedFragment() {
    val host = newBundle("host")
    host activator (classOf[TestActivator])
    host importPackage (classOf[BundleActivator].getPackage)
    host importPackage (classOf[ScalaObject].getPackage)

    val fragment = newBundle("fragment")
    fragment fragmentHost ("host")
    fragment addClass (classOf[TestService])
    fragment importPackage (classOf[ScalaObject].getPackage)

    val h = host.mkTmpFile()
    val f = fragment.mkTmpFile()

    val bh = ctx.installBundle(h.toURI.toURL.toExternalForm)
    val bf = ctx.installBundle(f.toURI.toURL.toExternalForm)

    bh.start()

    assertNotNull(ctx.getServiceReference(classOf[TestService].getName))

    // TODO tidy up copy paste from above - all the same bar uninstall fragment
    bf.uninstall()

    assertNull(ctx.getServiceReference(classOf[TestService].getName))
  }

  @Test
  @Ignore("Test not yet implemented")
  def attachFragmentAlways() {
    fail("Not testing " + Constants.FRAGMENT_ATTACHMENT_ALWAYS)
  }

  @Test
  @Ignore("Test not yet implemented")
  def attachFragmentNever() {
    fail("Not testing " + Constants.FRAGMENT_ATTACHMENT_NEVER)
  }

  @Test
  @Ignore("Test not yet implemented")
  def attachFragmentResolve() {
    fail("Not testing " + Constants.FRAGMENT_ATTACHMENT_RESOLVETIME)
  }

  @Test
  @Ignore("Test not yet implemented")
  def extensionBootClasspath() {
    fail("Not testing " + Constants.EXTENSION_BOOTCLASSPATH)
  }

  @Test
  @Ignore("Test not yet implemented")
  def extensionFramework() {
    fail("Not testing " + Constants.EXTENSION_FRAMEWORK)
  }

  @Test
  def getBundles() {
    val bundles = ctx.getBundles
    assertFalse(bundles.isEmpty)
  }

  @Test
  def getBundleByID() {
    var b = ctx.getBundle(0)

    assertNotNull(b)

    val bld = newBundle("foo")

    val f = bld.mkTmpFile()

    ctx.installBundle(f.toURI.toURL.toExternalForm)

    b = ctx.getBundle(1)

    assertNotNull(b)
  }


  @Test
  def getBundleByLocation() {
    var b = ctx.getBundle(Constants.SYSTEM_BUNDLE_LOCATION)

    assertNotNull(b)

    assertEquals(0, b.getBundleId)

    val bld = newBundle("foo")

    val f = bld.mkTmpFile()

    val location = f.toURI.toURL.toExternalForm

    ctx.installBundle(location)

    b = ctx.getBundle(location)

    assertNotNull(b)
  }

  @Test
  def getEntry() {
    val bld = newBundle("foo")
    bld addResource ("foo.txt", "wibble")

    val f = bld.mkTmpFile()

    val b = framework.getBundleContext.installBundle(f.toURI.toURL.toExternalForm)

    val res = b.getEntry("/foo.txt")

    assertNotNull(res)

    assertContents(res, "wibble")
  }

  @Test
  def getRootEntry() {
    val bld = newBundle("foo")
    val f = bld.mkTmpFile()

    val b = framework.getBundleContext.installBundle(f.toURI.toURL.toExternalForm)

    val res = b.getEntry("/")

    assertNotNull(res)
  }

  @Test
  def getNestedJarEntry() {
    val foo = newBundle("foo")
    foo addResource ("foo.txt", "wibble")
    val f = foo.mkTmpFile()

    val bar = newBundle("bar")
    bar addResource ("foo.jar", f)
    val b = bar.mkTmpFile()

    val bu = framework.getBundleContext.installBundle(b.toURI.toURL.toExternalForm)

    val root = bu.getEntry("/")

    assertNotNull(root)

    val nestedJar = root.toString + "foo.jar"

    val fu = framework.getBundleContext.installBundle(nestedJar)

    val res = fu.getEntry("/foo.txt")

    assertNotNull(res)

    assertContents(res, "wibble")
  }

  @Test
  @Ignore("work in progress")
  def requireBundleLookupCycle {
    val bundleA = newBundle("a")
    bundleA.activator(classOf[ActivatorA])
    bundleA.importPackage(classOf[BundleActivator].getPackage)
    bundleA.importPackage("scala")
    bundleA.exportPackage("resources")
    bundleA.requireBundle("b", Map("visibility" -> "reexport"))
    bundleA.requireBundle("c", Map("visibility" -> "reexport"))
    bundleA.addResource("resources/resource.txt", "resourceA")
    bundleA.addResource("resources/resource.a.txt", "resourceA")

    val bundleB = newBundle("b")
    bundleB.activator(classOf[ActivatorB])
    bundleB.exportPackage("resources", Map("version" -> "1.0.0", "myattribute" -> "foo"), Map("mandatory" -> "version,myattribute"))
    bundleB.importPackage(classOf[BundleActivator].getPackage)
    bundleB.importPackage("scala")
    bundleB.addResource("resources/resource.txt", "resourceB")
    bundleB.addResource("resources/resource.b.txt", "resourceB")

    val bundleC = newBundle("c")
    bundleC.importPackage(classOf[BundleActivator].getPackage)
    bundleC.importPackage("scala")
    bundleC.activator(classOf[ActivatorC])
    bundleC.exportPackage("resources")
    bundleC.requireBundle("a")
    bundleC.addResource("resources/resource.txt", "resourceC")

    val fb = framework.getBundleContext.installBundle(bundleB.mkTmpFile().toURI.toURL.toExternalForm)
    val fc = framework.getBundleContext.installBundle(bundleC.mkTmpFile().toURI.toURL.toExternalForm)
    val fa = framework.getBundleContext.installBundle(bundleA.mkTmpFile().toURI.toURL.toExternalForm)

    fb.start()
    fc.start()
    fa.start()

    val url = fa.getResource("resources/resource.txt")
    assertContents(url, "resourceC")

    fa.stop()
    fc.stop()
    fb.stop()
  }

  private def assertContents(url: URL, contents: String*) {
    assertNotNull(url)

    val src = Source.fromURL(url)

    val lines = src.getLines().toSet
    assertFalse("Invalid contents expected " + contents.toSet + " found " + lines, lines.filter(contents.contains(_)).isEmpty)

    src.close()
  }

  private val root = "/tmp/modula/" + Random.nextInt()

  private val config = Map[String, String](
    Constants.FRAMEWORK_STORAGE -> root,
    Constants.FRAMEWORK_BUNDLE_PARENT -> FrameworkFactoryImpl.NullParent
  )

  private def newFramework() = {
    val framework = new FrameworkFactoryImpl().newFramework(config)
    framework.start()
    framework
  }

}

class ActivatorA extends BundleActivator {
  def stop(ctx: BundleContext) {}

  def start(ctx: BundleContext) {}
}

class ActivatorB extends BundleActivator {
  def stop(ctx: BundleContext) {}

  def start(ctx: BundleContext) {}
}

class ActivatorC extends BundleActivator {
  def stop(ctx: BundleContext) {}

  def start(ctx: BundleContext) {}
}
