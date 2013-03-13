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

import collection.immutable.HashSet
import collection.mutable.{HashSet => MutableHashSet}
import FrameworkAction._
import org.chronologicalthought.modula.Constants._
import org.chronologicalthought.modula.{ServiceReference, ServiceRegistration}

/**
 * @author David Savage
 */

trait ServiceProvider extends ModuleContextEventProducer {
  private[impl] val registeredServices = new MutableHashSet[ServiceRegistrationImpl[_]]

  private[impl] def register[S](context: ModuleContextImpl, service: S, interfaces: Set[Class[_]], attributes: Map[String, Any]): ServiceRegistrationImpl[S] = {
    writeLock {
      val sid: Long = registeredServices.size + 1

      val registered = new ServiceRegistrationImpl[S](this, context, service, interfaces, attributes + (ServiceID -> sid))

      registeredServices += registered

      notifyListeners(registered, Registered)

      registered
    }
  }

  private[impl] def unregister(registration: ServiceRegistrationImpl[_]): Boolean = {
    writeLock {
      if (registeredServices.contains(registration)) {
        notifyListeners(registration, Unregistering)
        registeredServices.remove(registration)
        true
      } else {
        false
      }
    }
  }

  private[impl] def isRegistered(registration: ServiceRegistrationImpl[_]) = {
    readLock {
      registeredServices.contains(registration)
    }
  }

  private[impl] def references[T](context: ModuleContextImpl, clazz: Class[T], filter: Map[String, Any] => Boolean): Traversable[ServiceReference[T]] = {
    filterReferences(context, filter) {
      registration => clazz == null || registration.interfaces.exists {
        interface => {
          clazz.isAssignableFrom(interface)
        }
      }
    }
  }

  private[impl] def anyReferences(context: ModuleContextImpl, clazz: String, filter: Map[String, Any] => Boolean): Traversable[ServiceReference[AnyRef]] = {
    filterReferences(context, filter) {
      registration => clazz == null || registration.interfaces.map(_.getName).contains(clazz)
    }
  }

  private def filterReferences[T](context: ModuleContextImpl, filter: Map[String, Any] => Boolean)(registrationFilter: (ServiceRegistrationImpl[_] => Boolean)) = {
    def registered: HashSet[ServiceRegistrationImpl[_]] = {
      readLock {
        new HashSet() ++ registeredServices
      }
    }

    val found =
      for {
        registration <- registered
        if (registrationFilter(registration))
        if (serviceHooksAccept(registration))
        if (filter(registration.attributes))
        castReg = registration.asInstanceOf[ServiceRegistrationImpl[T]]
      }
      yield new ServiceReferenceImpl[T](context.module, castReg)

    found.toTraversable
  }

  // TODO implement serviceHookAccepts
  private def serviceHooksAccept(registration: ServiceRegistration[_]): Boolean = true
}
