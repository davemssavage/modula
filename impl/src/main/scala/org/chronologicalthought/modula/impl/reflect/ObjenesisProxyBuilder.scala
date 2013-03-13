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

import org.objenesis.ObjenesisStd
import net.sf.cglib.proxy.{Factory, NoOp, Enhancer}
import org.chronologicalthought.modula.reflect.ProxyBuilder
import java.lang.reflect.InvocationHandler
import org.chronologicalthought.modula.{Failure, Empty, Full, Box}

/**
 * @author David Savage
 */

// TODO could be a build step to create this proxy/service link?
// removes need to dynamically generate classes
class ObjenesisProxyBuilder extends ProxyBuilder {
  private val objenesis = new ObjenesisStd()

  def newProxy[S](service: S, interfaces: Set[Class[_]], handler: InvocationHandler): Box[S] = {
    try {
      service match {
        case an: AnyRef => {
          val cl = an.getClass.getClassLoader
          val supers = interfaces.filterNot(_.isInterface())

          if (!supers.isEmpty) {
            buildCGLibProxy(an, interfaces -- supers, cl)
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

  private def buildCGLibProxy[S](service: AnyRef, all: Set[Class[_]], cl: ClassLoader) = {
    // TODO need to use jdave unfinalizer to 'fix' scala classes?
    val enhancer = new Enhancer
    enhancer.setSuperclass(service.getClass)
    enhancer.setInterfaces(all.toArray)
    enhancer.setCallbackType(classOf[NoOp])
    val clazz = enhancer.createClass()

    val instance = objenesis.newInstance(clazz).asInstanceOf[Factory]
    instance.setCallbacks(Array(NoOp.INSTANCE))
    Full(instance.asInstanceOf[S])
  }
}
