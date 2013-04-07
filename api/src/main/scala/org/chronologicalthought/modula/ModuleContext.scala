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

import event.{ServiceEvent, ModuleContextListener}
import ref.Reference
import reflect.GarbageCollector
import reflect.ProxyBuilder.{identity, nullOp}
import java.lang.IllegalStateException
import collection.mutable.{HashSet, HashMap}

/**
 * @author David Savage
 */
trait ModuleContext {
  /**
   * The module with which this context is associated
   */
  def module: Module

  def property(key: String): Option[Any]

  def register[S](service: S, interfaces: Traversable[Class[_]], attributes: Traversable[(String, Any)] = List.empty): ServiceRegistration[S]

  // TODO services should be ordered by ServiceComparators
  // ServiceComparators may be stacked ordered by priority
  // of service comparators
  // TODO document isAssignable check
  def findReferences[T](clazz: Class[T], filter: Map[String, Any] => Boolean): Traversable[ServiceReference[T]]

  // TODO usage of word ANY is overloaded here - confusing
  // use Any below to mean find me any (happens to be first) service of this type
  // here it means find me a type and don't check classes are assignable
  def findAnyReferences(clazz: String, filter: Map[String, Any] => Boolean): Traversable[ServiceReference[AnyRef]]

  def registered: Traversable[ServiceReference[AnyRef]]

  def inUse: Traversable[ServiceReference[AnyRef]]

  // register helper methods

  def register[S](service: S): ServiceRegistration[S] = {
    register(service, List(service.asInstanceOf[AnyRef].getClass), List.empty)
  }

  def register[S](service: S, interface: Class[_]): ServiceRegistration[S] = {
    register(service, List(interface), List.empty)
  }

  def register[S](service: S, interface: Class[_], attributes: Traversable[Tuple2[String, Any]]): ServiceRegistration[S] = {
    register(service, List(interface), attributes)
  }

  def register[S](service: S, attributes: Traversable[Tuple2[String, Any]]): ServiceRegistration[S] = {
    register(service, List(service.asInstanceOf[AnyRef].getClass), attributes)
  }

  // reference helper methods
  def findReferences[T](clazz: Class[T]): Traversable[ServiceReference[T]] = {
    findReferences(clazz, _ => true)
  }

  def findAnyReferences(clazz: String): Traversable[ServiceReference[AnyRef]] = {
    findAnyReferences(clazz, _ => true)
  }

  def anyReference[S](clazz: Class[S]): Box[ServiceReference[S]] = {
    anyReference(clazz, _ => true)
  }

  def anyReference[S](clazz: Class[S], filter: Map[String, Any] => Boolean): Box[ServiceReference[S]] = {
    val t = findReferences(clazz, filter)
    if (t.isEmpty) Empty else Full(t.head)
  }

  // rich functional helper methods
  def withAny[T, R](clazz: Class[T])(f: (T => R)): Box[R] = {
    val refs = findReferences(clazz)
    val start: Box[(T, ServiceReference[T])] = Empty

    def firstServiceWithReference[S](found: Box[(S, ServiceReference[S])], nextReference: ServiceReference[S]) = {
      found.or(nextReference.get().map((_, nextReference)))
    }

    refs.foldLeft(start)(firstServiceWithReference).map(serviceAndReference => {
      try {
        f(serviceAndReference._1)
      }
      finally {
        serviceAndReference._2.unget(serviceAndReference._1)
      }
    })
  }

  // TODO don't like the name of this method, but it is very useful...
  def withAnyFlatten[T, R](clazz: Class[T])(f: (T => Box[R])): Box[R] = {
    val refs = findReferences(clazz)
    val start: Box[(T, ServiceReference[T])] = Empty

    def firstServiceWithReference[S](found: Box[(S, ServiceReference[S])], nextReference: ServiceReference[S]) = {
      found.or(nextReference.get().map((_, nextReference)))
    }

    refs.foldLeft(start)(firstServiceWithReference).flatMap(serviceAndReference => {
      try {
        f(serviceAndReference._1)
      }
      finally {
        serviceAndReference._2.unget(serviceAndReference._1)
      }
    })
  }

  def withEach[T, R](clazz: Class[T])(f: (T => R)): Traversable[R] = {
    withEach(clazz, _ => true)(f)
  }

  def withEach[T, R](clazz: Class[T], filter: Map[String, Any] => Boolean)(f: (T => R)): Traversable[R] = {
    val refs = findReferences(clazz, filter)
    refs.par.map(_.apply(f)).filter(!_.isEmpty).map(_.openOrThrowException("Filter failed to filter empty references")).seq
  }

  // gc helpers
  def anyService[S](clazz: Class[S]): Box[S] = {
    anyService(clazz, _ => true)
  }

  def anyService[S](clazz: Class[S], filter: Map[String, Any] => Boolean): Box[S] = {
    val start: Box[S] = Empty

    def firstService[S](found: Box[S], nextReference: ServiceReference[S]) = {
      def makeProxy(service: S) = buildProxy(service, nextReference)
      found.or(nextReference.get()).flatMap(makeProxy)
    }

    findReferences(clazz, filter).foldLeft(start)(firstService)
  }

  def findServices[T](clazz: Class[T]): Traversable[T] = {
    findServices(clazz, _ => true)
  }

  def findServices[T](clazz: Class[T], filter: Map[String, Any] => Boolean): Traversable[T] = {
    val refs = findReferences(clazz, filter)

    def proxy(ref: ServiceReference[T]) = ref.get().map(buildProxy(_, ref))

    val found = refs.flatMap(proxy(_))

    found.collect {
      case Full(service: T) => service
    }
  }

  // TODO to much junk in api class think this should be ported to an internal service of framework?
  private val serviceToReferences = new HashMap[Reference[AnyRef], ServiceReference[AnyRef]]()
  private val referenceToServices = new HashMap[ServiceReference[AnyRef], HashSet[Reference[AnyRef]]]()

  private def buildProxy[S](service: S, serviceReference: ServiceReference[S]): Box[S] = {
    val anyReference = serviceReference.asInstanceOf[ServiceReference[AnyRef]]
    val interfaces = serviceReference.interfaces

    def link(javaRef: Reference[AnyRef]) {
      serviceToReferences.synchronized {
        val set = referenceToServices.getOrElseUpdate(anyReference, new HashSet[Reference[AnyRef]]())
        set += javaRef
        serviceToReferences += javaRef -> anyReference
      }
    }

    def unlink(removed: Reference[AnyRef]) {
      val unget = serviceToReferences.synchronized {
        val last =
          referenceToServices.get(anyReference) match {
            case Some(services) => {
              services -= removed
              services.isEmpty
            }
            case None => true
          }

        if (last) {
          referenceToServices.remove(anyReference)
        }

        last
      }

      if (unget) serviceReference.unget(service)
    }

    // TODO should reuse gc proxy - keep creating proxies, only need to unget when all callers have stopped holding references
    withAny(classOf[GarbageCollector])(gc => {
      identity(service, interfaces, this).map(proxy => {
        val anyProxy = proxy.asInstanceOf[AnyRef]

        link(gc.weakRefHook(anyProxy) {
          unlink(_)
        })

        proxy
      }).openOr(service)
    })
  }

  // module helpers

  def findModule(id: Long): Box[Module] = {
    if (id == module.id) Full(module)
    else {
      def matchModuleId(attrs: Map[String, Any]) = attrs.get(Constants.ModuleID) match {
        case Some(v) => id == v
        case None => false
      }
      findServices(classOf[Module], matchModuleId).headOption
    }
  }

  // TODO...think about withModule more...
  //  def withModule[R](select: (Module) => Boolean)(f: (Module => R)): Option[R] = {
  //    moduleContextToSimpleModuleContext(this).withEach(classOf[Module]) {
  //      m => {
  //        if (select(m)) {
  //          f(m)
  //        }
  //      }
  //    }
  //  }

  def modules: Map[Long, Module] = {
    // TODO should this be a separate trait?
    findServices(classOf[Module]).map(s => s.id -> s).toMap
  }
}

class ModuleContextWrapper(underlying: ModuleContext) extends ModuleContext {
  def module = underlying.module

  def property(key: String): Option[Any] = underlying.property(key)

  def register[S](service: S, interfaces: Traversable[Class[_]], attributes: Traversable[(String, Any)] = List.empty): ServiceRegistration[S] = {
    underlying.register(service, interfaces, attributes)
  }

  def findReferences[T](clazz: Class[T], filter: Map[String, Any] => Boolean): Traversable[ServiceReference[T]] = {
    underlying.findReferences(clazz, filter)
  }

  def findAnyReferences(clazz: String, filter: Map[String, Any] => Boolean): Traversable[ServiceReference[AnyRef]] = {
    underlying.findAnyReferences(clazz, filter)
  }

  def registered: Traversable[ServiceReference[AnyRef]] = underlying.registered

  def inUse: Traversable[ServiceReference[AnyRef]] = underlying.inUse
}

// service helper methods
// TODO split these functions up into more traits?
trait SimpleModuleContext extends ModuleContext {
  thisContext =>

  def whenRegistered[T](clazz: Class[T], filter: (Map[String, Any] => Boolean))(f: (Pair[T, Map[String, Any]] => Any)): ServiceRegistration[ModuleContextListener] = {
    // TODO implement!
    // TODO use registerMy to gc when this context is gc'd
    throw new IllegalStateException("Not yet implemented")
  }

  def whenRegistered[T](clazz: Class[T])(f: (T => Any)): ServiceRegistration[ModuleContextListener] = {
    // TODO implement!
    // TODO use registerMy to gc when this context is gc'd
    throw new IllegalStateException("Not yet implemented")
  }

  def whenUnregistered[T](clazz: Class[T])(f: (T => Unit)): ServiceRegistration[ModuleContextListener] = {
    // TODO implement!
    // TODO use registerMy to gc when this context is gc'd
    throw new IllegalStateException("Not yet implemented")
  }

  def whenUnregistered[T](clazz: Class[T], filter: (Map[String, Any] => Boolean))(f: (Pair[T, Map[String, Any]] => Any)): ServiceRegistration[ModuleContextListener] = {
    // TODO implement!
    // TODO use registerMy to gc when this context is gc'd
    throw new IllegalStateException("Not yet implemented")
  }

  def track[T](clazz: Class[T])(f: PartialFunction[ServiceEvent, Unit]): ServiceRegistration[ModuleContextListener] = {
    // TODO implement!
    // TODO use registerMy to gc when this context is gc'd
    throw new IllegalStateException("Not yet implemented")
  }

  def registerMy[S](service: S): ServiceRegistration[S] = {
    // TODO use annotations to introspect vals/refs for service attributes
    // TODO implement!
    // use GarbageCollector to clearup any temporary registrations made by this context
    throw new IllegalStateException("Not yet implemented")
  }

  // TODO following two methods have stupid names...
  def anyServiceOrNullOp[S](clazz: Class[S]): S = {
    def buildNullOp = nullOp(clazz, thisContext).getOrElse(throw new IllegalStateException("Failed to create null op for " + clazz))
    anyServiceOrElse(clazz, buildNullOp)
  }

  def anyServiceOrElse[S](clazz: Class[S], ifNone: => S): S = {
    anyReference(clazz).map(_.get()).openOr(ifNone).asInstanceOf[S]
  }
}
