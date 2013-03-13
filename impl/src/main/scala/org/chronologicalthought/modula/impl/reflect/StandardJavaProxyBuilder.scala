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

package org.chronologicalthought.modula.impl.reflect

import org.chronologicalthought.modula.reflect.ProxyBuilder
import java.lang.reflect.{InvocationTargetException, Proxy, Method, InvocationHandler}
import org.chronologicalthought.modula.{Failure, Empty, Full, Box}
import scala.Array

/**
 * @author David Savage
 */

class StandardJavaProxyBuilder extends ProxyBuilder {
  def newProxy[S](service: S, interfaces: Set[Class[_]], handler: InvocationHandler): Box[S] = {
    try {
      service match {
        case an: AnyRef => {
          val cl = an.getClass.getClassLoader
          val supers = interfaces.filterNot(_.isInterface())

          if (supers.isEmpty) {
            buildStandardJavaProxy(an, interfaces, cl)
          }
          else {
            Empty
          }
        }
        case _ => Empty
      }
    }
    catch {
      case e => {
        // TODO framework logging
        println(service.getClass.getConstructors.toList)
        e.printStackTrace()
        Failure("Unable to proxy %s".format(service), Full(e), Empty)
      }
    }
  }

  private def buildStandardJavaProxy[S](service: AnyRef, all: Set[Class[_]], cl: ClassLoader) = {
    val handler = new InvocationHandler {
      def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]) = {
        // TODO is this sensible? fixes issues like equals comparing wrong object but seems a little clunky
        val unwrapped = if (args == null) {
          null
        } else {
          args.map(a => {
            if (a eq proxy) {
              service
            }
            else {
              a
            }
          })
        }
        try {
          method.invoke(service, unwrapped: _*)
        }
        catch {
          case e: InvocationTargetException => throw e.getCause
        }
      }
    }
    Full(Proxy.newProxyInstance(cl, all.toArray, handler).asInstanceOf[S])
  }
}
