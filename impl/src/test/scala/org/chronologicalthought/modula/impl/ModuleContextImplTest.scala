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
import org.junit.{Before, Ignore, Test}
import org.chronologicalthought.modula.{Framework, ServiceReference, ModuleContext}
import java.util.concurrent.atomic.AtomicInteger

/**
 * @author David Savage
 */

class ModuleContextImplTest {
  var framework: Framework = _
  var context: ModuleContext = _

  @Before
  def init() {
    framework = FrameworkFactoryImpl.newFramework()
    context = framework.context
    framework.start()
  }

  @Test
  def testRegister1() {
    val svc = new TestService
    context.register(svc)
    assertFoundService(svc, classOf[TestService])
    assertFoundService(svc, classOf[TestIt])
  }

  @Test
  def testRegister2() {
    val svc = new TestService
    context.register(svc, classOf[TestIt])
    assertFoundService(svc, classOf[TestIt])
    assertNotFoundService(classOf[TestService])
  }

  @Test
  def testRegister3() {
    val svc = new TestService
    context.register(svc, List(classOf[TestIt], classOf[TestService]))
    assertFoundService(svc, classOf[TestIt])
    assertFoundService(svc, classOf[TestService])
  }

  @Test
  def testRegister4() {
    val svc = new TestService
    context.register(svc, Map("wibble" -> 2))
    val leasedService = assertFoundService(svc, classOf[TestService])
    assertEquals(2, leasedService.attributes("wibble"))


    var leased = context.findReferences(classOf[TestService], _.contains("wibble"))
    assertFalse(leased.isEmpty)
    assertSame(svc, leased.head.get().openOr(throw new AssertionError("Missing service")))


    leased = context.findReferences(classOf[TestService], _("wibble") == 2)
    assertFalse(leased.isEmpty)
    assertSame(svc, leased.head.get().openOr(throw new AssertionError("Missing service")))

    leased = context.findReferences(classOf[TestService], _("wibble") == 3)
    assertTrue(leased.isEmpty)
  }

  @Test
  def testRegisterWithLease() {
    val svc = new TestService()
    val lease = context.register(svc)
    assertNotNull(lease)

    assertFoundService(svc, classOf[TestService])

    lease.unregister

    assertNotFoundService(classOf[TestService])
  }

  @Test
  def registered() {
    val test = new TestService
    val reg = context.register(test)
    assertTrue(context.registered.exists(_.get().map(_ == test).openOr(false)))
    reg.unregister()
    assertFalse(context.registered.exists(_.get().map(_ == test).openOr(false)))
  }

  @Test
  @Ignore("work in progress")
  def contextWhenDiscovered() {
    val count = new AtomicInteger()

    this.context.register(new TestService(), Set(classOf[TestIt]))

//    this.context.whenRegistered(classOf[TestIt]) {
//      // TODO figure out how to remove test from args
//      // probably not the majority use case but nice syntactic sugar
//      test => count.incrementAndGet()
//    }

    this.context.register(new TestService(), Set(classOf[TestIt]))

    assertEquals(2, count.get)
  }

  private def assertNotFoundService(interface: Class[_]) {
    val leased = context.anyReference(interface)
    assertTrue("Unexpected service", leased.isEmpty)
  }

  private def assertFoundService[T](svc: AnyRef, interface: Class[T]): ServiceReference[T] = {
    val refBox = context.anyReference(interface)

    val ref = refBox.openOr(throw new AssertionError("No such service"))

    val s = ref.get().openOr("Missing service")

    assertSame(svc, s)

    ref
  }
}

trait TestIt {
  def doIt(): Unit = {}
}

class TestService extends TestIt
