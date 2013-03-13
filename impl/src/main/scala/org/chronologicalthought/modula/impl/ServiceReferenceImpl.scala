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

import org.chronologicalthought.modula.{ServiceReference, Module}

/**
 * @author David Savage
 */

private[impl] class ServiceReferenceImpl[S](val module: Module, val registration: ServiceRegistrationImpl[S]) extends ServiceReference[S] {
  assert(module != null)
  assert(registration != null)

  def isAvailable = !registration.isUnregistered

  def interfaces = registration.interfaces

  def get = registration.getService(module)

  def unget(service: S) = registration.ungetService(module, service)

  def attributes = registration.attributes

  override def toString = {
    module.name + ":" + module.version + "->" + registration.service + ":" + registration.attributes
  }
}
