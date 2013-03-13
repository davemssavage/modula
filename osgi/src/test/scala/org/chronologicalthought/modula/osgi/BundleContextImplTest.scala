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
import org.junit.{Ignore, Test}
import java.util.{HashMap, Dictionary, Hashtable, Collections}
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.osgi.framework._
import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock
import actors.threadpool.LinkedBlockingQueue
import scala.Array
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.CountDownLatch
import java.lang.RuntimeException

/**
 * @author David Savage
 */

class BundleContextImplTest {

  trait Foo {
    def value: Int
  }

  class FooImpl(val value: Int) extends Foo {
    def this() = this (0)
  }

  @Test
  @Ignore("Test not yet implemented")
  def getProperty() {
    // TODO framework properties should delegate to system properties if not set
    // TODO equinox has the concept of snapshotting sys.props
    // at startup so they can't be changed - seems like an interesting idea
    // argument is that system properties have no events so allowing them to change at runtime is a mistake
    // should default be true or false for snapshot?
    // need a number of test scenarios here
    fail("Not yet testing properties")
  }

  @Test
  def registerService {
    val f = newFramework
    val ctx = f.getBundleContext

    val foo = new FooImpl()

    val reg = ctx.registerService(classOf[Foo].getName(), foo, null)

    val ref = ctx.getServiceReference(classOf[Foo].getName())

    assertNotNull(ref)

    val service = ctx.getService(ref)

    assertSame(foo, service)

    assertFalse(ctx.ungetService(ref))
  }

  @Test
  def getUnGetServiceWithString {
    val f = newFramework
    val ctx = f.getBundleContext

    val foo = new FooImpl()

    val reg = ctx.registerService(classOf[Foo].getName(), foo, null)

    val ref = ctx.getServiceReference(classOf[Foo].getName())

    assertNotNull(ref)

    val service1 = ctx.getService(ref)

    assertSame(foo, service1)

    val service2 = ctx.getService(ref)

    assertSame(foo, service2)

    assertTrue(ctx.ungetService(ref))

    assertFalse(ctx.ungetService(ref))

    val service3 = ctx.getService(ref)

    assertSame(foo, service3)

    assertFalse(ctx.ungetService(ref))
  }

  @Test
  def getUnGetServiceWithClass {
    val f = newFramework
    val ctx = f.getBundleContext

    val foo = new FooImpl()

    val reg = ctx.registerService(classOf[Foo], foo, null)

    val ref = ctx.getServiceReference(classOf[Foo])

    assertNotNull(ref)

    val service1 = ctx.getService(ref)

    assertSame(foo, service1)

    val service2 = ctx.getService(ref)

    assertSame(foo, service2)

    assertTrue(ctx.ungetService(ref))

    assertFalse(ctx.ungetService(ref))

    val service3 = ctx.getService(ref)

    assertSame(foo, service3)

    assertFalse(ctx.ungetService(ref))
  }

  @Test
  def compareServiceReferences() {
    val f = newFramework
    val ctx = f.getBundleContext

    val foo = new FooImpl()

    val reg = ctx.registerService(classOf[Foo].getName(), foo, null)

    val ref = ctx.getServiceReference(classOf[Foo].getName())

    assertEquals(0, ref.compareTo(ref))
  }

  @Test//(timeout = 5000)
  def getUnGetServiceWithServiceFactory {
    val f = newFramework
    val ctx = f.getBundleContext

    val getCount = new AtomicInteger
    val ungetCount = new AtomicInteger
    val ungetLatch = new CountDownLatch(1)

    val fooFactory = new ServiceFactory[Foo] {
      def getService(bundle: Bundle, fooRegistration: ServiceRegistration[Foo]) = {
        new FooImpl(getCount.incrementAndGet())
      }

      def ungetService(bundle: Bundle, fooRegistration: ServiceRegistration[Foo], foo: Foo) {
        ungetCount.incrementAndGet()
        ungetLatch.countDown()
      }
    }

    ctx.registerService(classOf[Foo].getName, fooFactory, null)

    val ref = ctx.getServiceReference(classOf[Foo])
    assertNotNull(ref)

    assertSame(1, ctx.getService(ref).value)
    assertSame(1, ctx.getService(ref).value)
    assertSame(1, ctx.getService(ref).value)

    assertEquals(0, ungetCount.get)
    // now start ungetting references
    assertTrue(ctx.ungetService(ref))
    assertEquals(0, ungetCount.get)
    assertTrue(ctx.ungetService(ref))
    assertEquals(0, ungetCount.get)
    assertFalse(ctx.ungetService(ref));
    assertFalse(ctx.ungetService(ref));

    // TODO shouldn't have to do this in OSGi api - wait internally for some event?
    // should use atomic boolean to check has been unlatched - or simple mock verify?
    ungetLatch.await()
    assertEquals(1, ungetCount.get)
  }

  @Test
  @Ignore("Test not yet implemented")
  def getUnGetServiceWithServiceFactoryWithGetException {
    val f = newFramework
    val ctx = f.getBundleContext

    val fooFactory = mock(classOf[ServiceFactory[Foo]])

    when(fooFactory.getService(isA(classOf[Bundle]), isA(classOf[ServiceRegistration[Foo]]))).thenThrow(new RuntimeException("Bang"))

    val reg = ctx.registerService(classOf[Foo].getName, fooFactory, null)

    var ref = ctx.getServiceReference(classOf[Foo])

    assertNotNull(ref)

    val service = ctx.getService(ref)

    assertNull(service)

    reg.unregister()

    ref = ctx.getServiceReference(classOf[Foo])

    assertNull(ref)

    fail("Not testing framework events")
  }

  @Test(timeout = 5000L)
  @Ignore("Test not yet implemented")
  def getUnGetServiceWithServiceFactoryWithUnGetException {
    val f = newFramework
    val ctx = f.getBundleContext

    val getCount = new AtomicInteger
    val ungetCount = new AtomicInteger
    val ungetLatch = new CountDownLatch(1)

    val fooFactory = new ServiceFactory[Foo] {
      def getService(bundle: Bundle, fooRegistration: ServiceRegistration[Foo]) = {
        new FooImpl(getCount.incrementAndGet())
      }

      def ungetService(bundle: Bundle, fooRegistration: ServiceRegistration[Foo], foo: Foo) {
        ungetCount.incrementAndGet()
        ungetLatch.countDown()
        throw new RuntimeException("Bang")
      }
    }

    ctx.registerService(classOf[Foo].getName, fooFactory, null)

    val ref = ctx.getServiceReference(classOf[Foo])

    assertNotNull(ref)

    assertEquals(1, ctx.getService(ref).value)

    ctx.ungetService(ref);

    // TODO shouldn't have to do this in OSGi api - wait internally for some event?
    ungetLatch.await()

    fail("Not testing framework events")
  }

  @Test
  def testFactoryRecursion() {
    val f = newFramework
    val ctx = f.getBundleContext
    val classes = Array(classOf[Foo].getName());
    val registration = ctx.registerService(
      classes, new ServiceFactory[Foo]() {
        val depth = new AtomicInteger();

        def getService(bundle: Bundle, reg: ServiceRegistration[Foo]) = {
          if (depth.incrementAndGet() == 2) {
            new FooImpl();
          }
          else {
            ctx.getService(reg.getReference());
          }
        }

        def ungetService(bundle: Bundle, reg: ServiceRegistration[Foo], service: Foo) {
          // empty
        }
      }, null);

    try {
      val reference = ctx.getServiceReference(
        classOf[Foo].getName());
      assertNotNull(reference);

      val service = ctx.getService(reference);
      assertNull(service);
    }
    finally {
      registration.unregister();
    }
  }

  // this test checks that underlying modula impl doesn't rely on gc to tidy up
  @Test
  def getServiceThenUnregisterServiceWhilstHoldingReference() {
    val f = newFramework
    val ctx = f.getBundleContext

    val foo = new FooImpl()

    val reg = ctx.registerService(classOf[Foo].getName(), foo, null)

    val ref = ctx.getServiceReference(classOf[Foo].getName())

    assertNotNull(ref)

    val service1 = ctx.getService(ref)
    assertSame(foo, service1)

    val service2 = ctx.getService(ref)
    assertSame(foo, service2)

    reg.unregister

    assertFalse(ctx.ungetService(ref))
  }

  @Test(timeout = 5000L)
  def addServiceListener {
    val f = newFramework
    val ctx = f.getBundleContext

    val queue = new LinkedBlockingQueue[ServiceEvent]
    val listener = mock(classOf[ServiceListener])
    when(listener.serviceChanged(isA(classOf[ServiceEvent]))).thenAnswer(new Answer[Unit] {
      def answer(p1: InvocationOnMock) = {
        queue.offer(p1.getArguments()(0).asInstanceOf[ServiceEvent])
      }
    })

    ctx.addServiceListener(listener)

    val foo = new FooImpl()

    val reg = ctx.registerService(classOf[Foo].getName(), foo, null)

    println(queue.take)
    println(queue.take)

    val props = new Hashtable[String, String]()
    props.put("foo", "bar")

    reg.setProperties(props)

    println(queue.take)

    reg.unregister

    println(queue.take)
  }

  @Test
  def getDataFile() {
    val f = newFramework
    val ctx = f.getBundleContext
    val f1 = ctx.getDataFile("foo")
    assertNotNull(f1)

    val f2 = f.getDataFile("foo")
    assertNotNull(f2)

    assertEquals(f1.getAbsolutePath, f2.getAbsolutePath)
  }


  private def newFramework() = {
    val framework = new FrameworkFactoryImpl().newFramework(Collections.emptyMap())
    framework.start()
    framework
  }
}
