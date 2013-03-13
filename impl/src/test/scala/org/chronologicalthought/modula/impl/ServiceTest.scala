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
import org.chronologicalthought.modula.{Module, ModuleContext, ModuleProvider}


/**
 * @author David Savage
 */

class ServiceTest {
  @Test
  def serviceUnregisteredOnBundleStop {
    def test() {
      val framework = FrameworkFactoryImpl.newFramework()
      framework.start()

      val provider = mock(classOf[ModuleProvider])

      framework.context.register(provider, classOf[ModuleProvider])

      var found = framework.context.findServices(classOf[Module])
      found = found.filter(_.isProvidedBy(provider))

      assertEquals(1, found.size)

      val module = found.head
      module.start()

      class TestService

      module.context.register(new TestService)

      def assertFindService(ctx: ModuleContext, find: Boolean) {
        val found = ctx.findAnyReferences(classOf[TestService].getName)
        if (find)
          assertEquals(1, found.size)
        else
          assertEquals(0, found.size)
      }

      assertFindService(framework.context, true)

      module.stop()

      assertFindService(framework.context, false)
    }

    for (i <- 1 to 100) test
  }
}
