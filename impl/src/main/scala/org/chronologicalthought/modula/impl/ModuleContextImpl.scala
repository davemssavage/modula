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

import org.chronologicalthought.modula.{Box, ServiceReference, Module, ModuleContext, ServiceRegistration}
import actors.Actor._
import java.util.concurrent.CountDownLatch
import collection.mutable.{ArrayBuffer, HashSet}

/**
 * @author David Savage
 */

private[impl] class ModuleContextImpl(framework: FrameworkImpl) extends ModuleContext {
  private var mod: Module = _

  private val registrations = new HashSet[ServiceRegistrationImpl[_]]

  override def register[S](service: S, interfaces: Traversable[Class[_]], attributes: Traversable[(String, Any)]): ServiceRegistration[S] = {
    val reg = framework.register(this, service, interfaces.toSet, attributes.toMap)

    registrations.synchronized {
      registrations += reg
    }

    reg
  }

  private[impl] def unregister(reg: ServiceRegistrationImpl[_]) {
    registrations.synchronized {
      registrations -= reg
    }
  }

  def findReferences[T](clazz: Class[T], filter: Map[String, Any] => Boolean): Traversable[ServiceReference[T]] = {
    framework.references(this, clazz, filter)
  }

  def findAnyReferences(clazz: String, filter: Map[String, Any] => Boolean): Traversable[ServiceReference[AnyRef]] = {
    framework.anyReferences(this, clazz, filter)
  }

  def registered: Traversable[ServiceReference[AnyRef]] = {
    val buf = new ArrayBuffer[ServiceRegistration[_]]

    registrations.synchronized {
      registrations.copyToBuffer(buf)
    }

    buf.map(reg => new ServiceReferenceImpl[AnyRef](mod, reg.asInstanceOf[ServiceRegistrationImpl[AnyRef]])).toList
    // TODO create test to check inUse and registered
  }

  def inUse: Traversable[ServiceReference[AnyRef]] = {
    // TODO implement inUse
    throw new IllegalStateException("Not yet implemented")
  }


  def property(key: String): Option[Any] = {
    framework.property(key)
  }

  def module: Module = synchronized {
    mod
  }

  def module_=(mod: Module) {
    synchronized {
      this.mod = mod
    }
  }

  private[impl] def unregisterAll() {
    val toClear = registrations.synchronized {
      val snapshot = registrations.toBuffer
      registrations.clear
      snapshot
    }

    framework.lockState match {
      case LockType.Synchronized => {
        toClear.foreach(_.unregister)
      }
      case LockType.Unsynchronized => {
        val done = new CountDownLatch(toClear.size)

        val stopActor = actor {
          loop {
            react {
              case reg: ServiceRegistration[_] => {
                try {
                  reg.unregister
                } finally {
                  done.countDown
                }
              }
            }
          }
        }

        toClear.foreach(stopActor ! _)

        done.await
      }
    }
  }

}
