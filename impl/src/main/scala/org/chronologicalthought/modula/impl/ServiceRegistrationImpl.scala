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

import ref.WeakReference
import collection.mutable.HashMap
import org.chronologicalthought.modula.{Constants, ServiceRegistration, ServiceFactory, Module}
import org.chronologicalthought.modula.{Empty, Full, Box}

/**
 * @author David Savage
 */

class ServiceRegistrationImpl[S](provider: ServiceProvider, context: ModuleContextImpl, initialService: S, val interfaces: Set[Class[_]], attrs: Map[String, Any])
  extends ServiceRegistration[S] {

  val id = attrs(Constants.ServiceID)
  assert(id != null)

  private val moduleMap = new HashMap[Module, WeakReference[AnyRef]]
  private var s = initialService
  private var currAttrs = attrs

  def attributes: Map[String, Any] = {
    synchronized(currAttrs)
  }

  def attributes_=(properties: Map[String, Any]) {
    properties.get(Constants.ServiceID) match {
      case Some(_) => throw new IllegalArgumentException("Attempt to reassign " + Constants.ServiceID)
      case None => {
        synchronized {
          currAttrs = properties + (Constants.ServiceID -> currAttrs(Constants.ServiceID))
          provider.notifyListeners(this, FrameworkAction.Modified)
        }
      }
    }
  }

  def isUnregistered: Boolean = {
    !provider.isRegistered(this)
  }

  def unregister(): Boolean = {
    context.unregister(this)
    provider.unregister(this)
  }

  def service = {
    synchronized {
      s
    }
  }

  def service_=(service: S) {
    synchronized {
      s = service
    }
    provider.notifyListeners(this, FrameworkAction.Modified)
  }

  def getService(module: Module): Box[S] = {
    provider.writeLock {
      if (provider.registeredServices.contains(this)) {
        service match {
          case factory: ServiceFactory[S] => {
            val result = factory.getService(module, this.asInstanceOf[ServiceRegistration[S]])
            result
          }
          case service => Full(service)
        }
      }
      else {
        Empty
      }
    }
  }

  def ungetService(module: Module, unserviced: S) {
    provider.writeLock {
      if (provider.registeredServices.contains(this)) {
        service match {
          case f: ServiceFactory[S] => {
            moduleMap.remove(module)
            f.ungetService(module, this, unserviced)
          }
          case _ => // no action
        }
      }
    }
  }

  def newReference(module: Module) = new ServiceReferenceImpl(module, this)

  override def toString = {
    val b = new StringBuilder
    b.append(service)
    b.append("\n")
    b.append(interfaces)
    b.append("\n")
    b.append(attributes)
    b.toString()
  }
}
