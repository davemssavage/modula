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
package org.chronologicalthought.modula

import java.net.URL
import concurrent.Future

/**
 * @author David Savage
 */
trait ModuleLike extends Part {
  def loadClass(name: String): Box[Class[_]]

  def attributes: Map[String, Any]

  /**
   * @param filter
   */
  def resources(filter: (String) => Boolean): Traversable[URL] = resources(filter, true, false)

  /**
   * Searches this module for resources
   * All resources are relative to /
   *
   * @param filter
   * @param resolve
   * @param local
   */
  // TODO change this from booleans to enum - booleans are confusing
  def resources(filter: (String) => Boolean, resolve: Boolean, local: Boolean): Traversable[URL]

  def start(): Future[ModuleLike]

  def stop(): Future[ModuleLike]
}