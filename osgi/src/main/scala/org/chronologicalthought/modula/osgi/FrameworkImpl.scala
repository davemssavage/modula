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

import java.io._
import org.osgi.framework.launch.Framework
import org.osgi.framework.{BundleException, Constants, FrameworkEvent}
import org.chronologicalthought.modula.{ModuleState, Module}

/**
 * @author David Savage
 */

class FrameworkImpl(config: Map[String, String], module: Module) extends AbstractBundle(config, module) with Framework {

  override val getSymbolicName = Constants.SYSTEM_BUNDLE_SYMBOLICNAME

  val getLocation = Constants.SYSTEM_BUNDLE_LOCATION

  val getLastModified = System.currentTimeMillis

  def init() = {}

  def waitForStop(timeout: Long): FrameworkEvent = {
    // TODO does wait for framework Resolved make sense?
    module.waitFor(ModuleState.Resolved, timeout)
    new FrameworkEvent(FrameworkEvent.STOPPED, this, null)
  }

  override def adapt[A](clazz: Class[A]): A = {
    throw new IllegalStateException("Not yet implemented")
  }

  override def update() = {
    stop()
    start()
  }

  override def start(options: Int) {
    super.start(options)
    // TODO use framework hooks to hide this service
    module.context.register(new BundleFactoryImpl(config, module.context), classOf[BundleFactory])
  }

  def update(input: InputStream) = {
    input.close()
    update
  }

  def uninstall() = {
    throw new BundleException("Framework cannot be uninstalled")
  }
}
