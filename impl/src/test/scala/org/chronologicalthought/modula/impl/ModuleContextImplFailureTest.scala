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

import org.chronologicalthought.modula.{Framework, ModuleContext}
import org.junit.{Before, Test}

/**
 * @author David Savage
 */

class ModuleContextImplFailureTest {

  var framework: Framework = _
  var context: ModuleContext = _

  @Before
  def init() {
    framework = FrameworkFactoryImpl.newFramework()
    context = framework.context
  }

  @Test(expected = classOf[IllegalStateException])
  def testRegisterNotStarted() {
    new TestService
    context.register(new AnyRef)
  }
}

