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

import org.chronologicalthought.modula._
import org.junit.Assert._
import org.junit.Test

/**
 * @author David Savage
 */

class ServiceRegistrationTest {
  @Test
  def swapServiceRegistration() {
    val framework = FrameworkFactoryImpl.newFramework()

    framework.start()

    class TestService

    val serviceA = new TestService

    val reg = framework.context.register(serviceA)

    assertFalse("Missing test service", framework.context.withEach(classOf[TestService]) {
      service => {
        assertSame(serviceA, service)
        service
      }
    }.isEmpty)

    val serviceB = new TestService

    reg.service_=(serviceB)

    assertFalse("Missing test service", framework.context.withEach(classOf[TestService]) {
      service => {
        assertSame(serviceB, service)
        service
      }
    }.isEmpty)
  }
}
