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

/**
 * @author David Savage
 */
// TODO this should extend capability but capability is a case class and this is a trait??
trait ServiceReference[T] {
  /**
   * The module that registered this service
   */
  def module: Module

  /**
   * The interfaces under which this service reference was registered with
   */
  def interfaces: Set[Class[_]]

  // TODO decide on names for these methods, get, unget and service are kind of lame
  def get(): Box[T]

  def unget(service: T)

  /**
   * Applies the supplied function to the service mapped to this reference
   * and returns the value if that service is available or none
   * if this service is not available for some reason.
   */
  def apply[R](f: (T => R)): Box[R] = {
    def apply(service: T) = {
      try {
        f(service)
      }
      finally {
        unget(service)
      }
    }
    get().map(apply)
  }

  def attributes: Map[String, Any]

  def isAvailable: Boolean
}
