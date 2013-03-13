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

import LockType._
import FrameworkAction._
import actors.Actor._
import org.chronologicalthought.modula._
import org.chronologicalthought.modula.event._


/**
 * @author David Savage
 */

private[impl] trait ModuleContextEventProducer extends FrameworkBase {

  private val listenerActor = actor {
    loop {
      react {
        case (registration: ServiceRegistrationImpl[_], action: FrameworkAction) => {
          handleRegistration(registration, action)
        }
        case (listener: ModuleContextListener, event: ModuleContextEvent) => {
          doNotify(listener, event)
        }
      }
    }
  }

  private[impl] def notifyListeners(registration: ServiceRegistrationImpl[_], action: FrameworkAction) {
    lockState match {
      case Synchronized => {
        handleRegistration(registration, action)
      }
      case Unsynchronized => {
        listenerActor ! (registration, action)
      }
    }
  }

  private def handleRegistration(registration: ServiceRegistrationImpl[_], action: FrameworkAction) {
    val listeners = contextListeners(matchAny)
    try {
      listeners.foreach(reference => {
        reference {
          listener => {
            val ref = registration.newReference(reference.module)

            val event = action match {
              case Registered => new ServiceRegisteredEvent(ref, registration.interfaces, registration.attributes)
              case Modified => new ServiceModifiedEvent(ref, registration.interfaces, registration.attributes)
              case Unregistering => new ServiceUnregisteringEvent(ref, registration.interfaces, registration.attributes)
            }

            lockState match {
              case Synchronized => doNotify(listener, event)
              case Unsynchronized => listenerActor ! (listener, event)
            }
          }
        }
      })
    }
    catch {
      // TODO log failure to notify event
      case e => e.printStackTrace()
      throw e
    }
  }

  private def doNotify(listener: ModuleContextListener, event: ModuleContextEvent) {
    if (listener.isDefinedAt(event)) {
      listener.apply(event)
    }
  }

  private def contextListeners(f: Map[String, Any] => Boolean): Traversable[ServiceReference[ModuleContextListener]] = {
    context.findReferences(classOf[ModuleContextListener], f)
  }

  private def matchAny(m: Map[String, Any]) = true

  private def matchLock(m: Map[String, Any]): Boolean = {
    import ModuleContextListener._

    lockState match {
      case Synchronized => m.get(Synchronous) match {
        case None => false
        case Some(b: Boolean) => b
      }
      case Unsynchronized => m.get(Synchronous) match {
        case None => true
        case Some(b: Boolean) => !b
      }
    }
  }
}
