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

package org.chronologicalthought.modula.reflect

import org.chronologicalthought.modula.{Box, Empty, ServiceReference, ModuleContext}
import java.lang.reflect.{Method, InvocationHandler}

/**
 * User: dave
 */

object ProxyBuilder {
  // TODO make ctx first param in all these methods?
  // TODO interfaces as var args?
  def proxy[S](svc: S, interfaces: Set[Class[_]], handler: InvocationHandler, ctx: ModuleContext): Box[S] = {
    def buildFrom[S](svc: S, builders: Traversable[ServiceReference[ProxyBuilder]]): Box[S] = {
      val start: Box[S] = Empty
      def buildProxy(proxied: Box[S], proxyBuilder: ServiceReference[ProxyBuilder]) = proxied.or(proxyBuilder.apply(_.newProxy(svc, interfaces, handler)).openOr(Empty))
      builders.foldLeft(start)(buildProxy)
    }

    buildFrom(svc, ctx.findReferences(classOf[ProxyBuilder]))
  }

  def nullOp[S](interface: Class[S], ctx: ModuleContext): Option[S] = {
    // TODO following causes compile error: overloaded method value nullOp with alternatives ??
    // nullOp(Set(interface), ctx)
    // TODO implement null mock
    None
  }

  def nullOp[S](interfaces: Set[Class[_]], ctx: ModuleContext): Option[S] = {
    // TODO implement null mock
    None
  }

  def identity[S](svc: S, interfaces: Set[Class[_]], ctx: ModuleContext): Box[S] = {
    val handler = new InvocationHandler {
      def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]) = {
        method.invoke(svc, args: _*)
      }
    }

    proxy(svc, interfaces, handler, ctx)
  }
}

trait ProxyBuilder {
  def newProxy[S](service: S, interfaces: Set[Class[_]], handler: InvocationHandler): Box[S]
}
