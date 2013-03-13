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
import org.mockito.Mockito._

import org.junit.Test
import FrameworkFactoryImpl._
import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock
import java.util.concurrent.{TimeUnit, Semaphore}
import java.util.concurrent.atomic.AtomicBoolean
import org.chronologicalthought.modula.{ModuleLike, ModuleContext, ModuleState, ModuleProvider}
import concurrent.Promise

/**
 * @author David Savage
 */

class FrameworkImplTest {

  @Test
  def testFrameworkStart() {
    val framework = newFramework()
    val context = framework.context
    framework.start()
    assertFalse(context.modules.isEmpty)
    assertSame(framework, context.findModule(0).openOr(throw new AssertionError("Missing framework module")))
  }

  @Test(expected = classOf[IllegalStateException])
  def testFrameworkStop() {
    val framework = newFramework()
    val context = framework.context

    framework.start()
    framework.stop()

    assertTrue(context.modules.isEmpty)
  }

  @Test
  def testWaitForStop() {
    def waitForStop() {
      val blockSemaphore = new Semaphore(1)
      val stoppedSemaphore = new Semaphore(1)

      stoppedSemaphore.acquire()
      blockSemaphore.acquire()

      val framework = newBlockingFramework(blockSemaphore)

      run {
        // TODO does wait for framework Resolved make sense?
        framework.waitFor(ModuleState.Resolved)
        stoppedSemaphore.release(1)
      }


      run {
        framework.stop()
      }

      assertFalse("Unexpectedly stopped prior to test", stoppedSemaphore.tryAcquire())

      blockSemaphore.release()

      assertTrue("Failed to stop framework", stoppedSemaphore.tryAcquire(1, TimeUnit.MINUTES))
    }

    for (i <- 1 to 100) {
      waitForStop()
    }
  }

  @Test
  def testWaitForStopTimeout() {
    val blockSemaphore = new Semaphore(1)
    val stoppedSemaphore = new Semaphore(1)

    stoppedSemaphore.acquire()
    blockSemaphore.acquire()

    val framework = newBlockingFramework(blockSemaphore)
    val result = new AtomicBoolean

    run {
      val success = framework.waitFor(ModuleState.Resolved, 100, TimeUnit.MILLISECONDS)

      result.set(success)

      stoppedSemaphore.release(1)
    }

    run {
      framework.stop()
    }

    assertTrue(stoppedSemaphore.tryAcquire(5, TimeUnit.SECONDS))

    assertFalse(result.get)
  }

  @Test
  def testFrameworkStartStart() {
    val framework = newFramework()
    val context = framework.context

    framework.start()
    assertEquals(1, context.modules.size)

    framework.start()
    assertEquals(1, context.modules.size)
  }

  private def run(f: => Unit) {
    val t = new Thread() {
      override def run() {
        f
      }
    }
    t.start()
  }

  private def newBlockingFramework(blockSemaphore: Semaphore) = {
    val framework = newFramework()

    framework.start()

    val blockingModule = mock(classOf[ModuleProvider])

    when(blockingModule.stop()).thenAnswer(new Answer[Promise[ModuleLike]]() {
      def answer(p1: InvocationOnMock) = {
        blockSemaphore.acquire()
        Promise.successful(blockingModule)
      }
    })

    framework.context.register(blockingModule, classOf[ModuleProvider])

    val module = assertAndGetModuleFrom(framework.context)(1)
    module.start()

    framework
  }


  private def assertAndGetModuleFrom(context: ModuleContext)(id: Long) = {
    context.findModule(id).openOr(throw new AssertionError("Missing module"))
  }

}
