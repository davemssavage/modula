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

package org.chronologicalthought.modula.factory

import org.chronologicalthought.modula.FrameworkFactory
import org.chronologicalthought.modula.impl.{FrameworkImplConstants, FrameworkFactoryImpl}

/**
 * @author David Savage
 */

object GlobalFactory extends FrameworkFactory {
  val ParentClassLoader = FrameworkImplConstants.ParentClassLoader
  val Capabilities = FrameworkImplConstants.Capabilities
  val ExtraCapabilities = FrameworkImplConstants.ExtraCapabilities

  def newFramework(config: Map[String, Any]) = {
    FrameworkFactoryImpl.newFramework(config)
  }
}
