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
import org.junit.Test
import FrameworkFactoryImpl._
import org.chronologicalthought.modula.reflect.GarbageCollector
import java.util.concurrent.{TimeUnit, Semaphore}
import ref.Reference
import collection.mutable.HashSet

/**
 * @author David Savage
 */

class GarbageCollectorImplTest {

  @Test //(timeout = 5000L)
  def weakHook() {
    val framework = newFramework()
    val context = framework.context
    framework.start()

    val done = new Semaphore(1)
    done.acquire()

    assertFalse("Missing garbage collector service", context.anyService(classOf[GarbageCollector]).map(_.weakHook(new AnyRef) {
      done.release()
    }).isEmpty)

    def waitForRelease() {
      for (i <- 1 to 100) new AnyRef
      System.gc()
      if (!done.tryAcquire(10, TimeUnit.SECONDS)) {
        waitForRelease()
      }
    }

    waitForRelease()
  }

  @Test //(timeout = 5000L)
  def weakRefHook() {
    val framework = newFramework()
    val context = framework.context
    framework.start()

    val done = new Semaphore(1)
    done.acquire()

    var hook = new AnyRef
    val refs = new HashSet[Reference[_]]()

    refs.add(context.withAny(classOf[GarbageCollector])({
      gc => {
        gc.weakRefHook(hook) {
          ref => {
            if(refs.remove(ref)) {
              done.release()
            }
          }
        }
      }
    }).openOr(throw new AssertionError("Missing garbage collector service")))

    def waitForRelease {
      for (i <- 1 to 100) new AnyRef
      System.gc()
      if (!done.tryAcquire(10, TimeUnit.SECONDS)) {
        waitForRelease
      }
    }

    hook = null

    waitForRelease
  }

}
