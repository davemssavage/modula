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

package org.chronologicalthought.modula.osgi

import org.chronologicalthought.{modula => mod}
import mod.Constants.ServiceID
import mod.LDAPExpr.expr
import mod.LDAPExpr.`=`
import org.osgi.{framework => osgi}

import java.util.Dictionary
import java.lang.String
import collection.JavaConversions

/**
 * @author David Savage
 */

class OSGiServiceRegistrationImpl[T](underlying: mod.ServiceRegistration[T], ctx: mod.ModuleContext) extends osgi.ServiceRegistration[T] {
  private lazy val selfReference = {
    // TODO this impl is clunky - there must be a better way to get the reference?
    val id = underlying.attributes.getOrElse(ServiceID, throw new IllegalStateException("Missing " + ServiceID))
    val e = expr(ServiceID, `=`, id.toString)
    val ref = ctx.anyReference(null, e).getOrElse(throw new IllegalStateException("Missing service for " + ServiceID + " " + id))
    ref.asInstanceOf[mod.ServiceReference[T]]
  }

  def unregister = {
    underlying.unregister
  }

  def setProperties(p1: Dictionary[String, _]) = {
    val newAttrs = JavaConversions.dictionaryAsScalaMap(p1.asInstanceOf[Dictionary[String, Any]])

    underlying.attributes.get(osgi.Constants.OBJECTCLASS) match {
      case Some(value) => {
        newAttrs += osgi.Constants.OBJECTCLASS -> value
      }
      case None => throw new IllegalStateException("Missing object class")
    }

    underlying.attributes_=(newAttrs.toMap)
  }

  def getReference: osgi.ServiceReference[T] = {
    new OSGiServiceReferenceImpl(selfReference)
  }
}
