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

package org.chronologicalthought.modula.osgi.test.activator

import org.osgi.framework.{BundleContext, BundleActivator}
import org.chronologicalthought.modula.osgi.test.api.TestService

/**
 * @author David Savage
 */
class TestActivator extends BundleActivator {
  def start(ctx: BundleContext) = {
    ctx.registerService(classOf[TestService].getName, new TestService(), null)
  }

  def stop(ctx: BundleContext) = {}
}
