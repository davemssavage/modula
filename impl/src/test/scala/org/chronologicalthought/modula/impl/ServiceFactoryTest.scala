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

import org.junit.Assert._
import org.junit.{After, Before, Ignore, Test}
import org.chronologicalthought.modula._
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import org.chronologicalthought.modula.Full

/**
 * @author David Savage
 */

class ServiceFactoryTest {
  var framework: Framework = _

  @Before
  def setup() {
    framework = FrameworkFactoryImpl.newFramework()
    framework.start()
  }

  @After
  def clearup {
    framework.stop()
    framework = null
  }

  @Test
  def getUngetFromServiceFactoryWithNoInterface() {
    class Foo {
      def foo() = "foo"
    }

    val factory = new ServiceFactory[Foo] {
      def getService(module: Module, registration: ServiceRegistration[Foo]) = {
        Full(new Foo)
      }

      def ungetService(module: Module, registration: ServiceRegistration[Foo], service: Foo) {
      }
    }

    val context = framework.context

    context.register(factory, classOf[Foo])

    val foo = context.anyService(classOf[Foo]).openOr(throw new AssertionError("Missing test service"))
    assertEquals("foo", foo.foo())
  }

  @Test
  @Ignore("Work in progress")
  def getUngetFromServiceFactoryWithNoInterfaceAndFinal() {
    final class Bar {
      def bar() = "bar"
    }

    val factory = new ServiceFactory[Bar] {
      def getService(module: Module, registration: ServiceRegistration[Bar]) = {
        Full(new Bar)
      }

      def ungetService(module: Module, registration: ServiceRegistration[Bar], service: Bar) {
      }
    }

    val context = framework.context

    context.register(factory, classOf[Bar])

    assertFalse(context.anyService(classOf[Bar]).isEmpty)
  }

  @Test //(timeout = 30000L)
  def getUngetFromServiceFactoryRepeatedly() {
    def getUngetFromServiceFactory() {
      val count = 2
      val factory = new TestFactory(count)

      val context = framework.context

      val reg = context.register(factory, classOf[TestCounter])

      def getServices(count: Int) = {
        val svs = for (
          i <- 1 to count
        ) yield {
          context.anyService(classOf[TestCounter]).openOr(throw new AssertionError("Missing service"))
        }
        svs.toList
      }

      try {
        var svs = getServices(count)
        factory.waitForGet()
        assertEquals(count, svs.length)
        factory.assertNoUnGets()
        svs = null
        factory.waitForUnGet()
      }
      finally {
        reg.unregister()
      }
    }

    for (i <- 1 to 10) {
      println("started " + i)
      getUngetFromServiceFactory
      println("finished " + i)
    }
  }

  val counter = new AtomicInteger()

  trait TestCounter {
    def value: Int
  }

  class TestCounterImpl extends TestCounter {
    val value = counter.incrementAndGet()

    override def toString = "TestCounter[" + value + "]"
  }

  class TestFactory(count: Int) extends ServiceFactory[TestCounter] {
    private val getLatch = new CountDownLatch(count)
    private val ungetLatch = new CountDownLatch(count)
    private val ungetCount = new AtomicInteger

    def getService(module: Module, registration: ServiceRegistration[TestCounter]) = {
      getLatch.countDown
      Some(new TestCounterImpl)
    }

    def ungetService(module: Module, registration: ServiceRegistration[TestCounter], service: TestCounter) {
      println("Unget " + service)
      ungetCount.incrementAndGet()
      ungetLatch.countDown
    }

    def assertNoUnGets() {
      assertEquals(0, ungetCount.get())
    }

    def waitForGet() {
      waitFor(getLatch, "Waiting for get")
    }

    def waitForUnGet() {
      waitFor(ungetLatch, "Waiting for unget")
    }

    private def waitFor(latch: CountDownLatch, msg: String) {
      System.gc
      var waiting = true
      while (!latch.await(1, TimeUnit.SECONDS)) {
        System.gc
        if (waiting) {
          print(msg)
          waiting = false
        }
        else print(".")
        Console.flush
      }
    }
  }

}
