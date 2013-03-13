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
import mod.{Constants => MC}
import org.osgi.{framework => osgi}
import osgi.{Constants => OC}
import java.lang.String

import actors.{Futures, Future}
import collection.mutable.{HashSet, SynchronizedSet}
import org.chronologicalthought.modula.{Full, Empty, Box}

/**
 * @author David Savage
 */

// TODO should references be mapped using weak hashmap vs new'd each time?
class OSGiServiceReferenceImpl[S](modRef: mod.ServiceReference[S]) extends osgi.ServiceReference[S] {
  private var serviceFuture: Option[GetOnce[Box[S]]] = None
  private var usage: Long = 0
  private val underlying = modRef

  override def toString = {
    underlying.toString
  }

  /**
   * Compares this ServiceReference with the specified ServiceReference for order.
   * If this ServiceReference and the specified ServiceReference have the
   * same service id they are equal.
   *
   * This ServiceReference is less than the specified ServiceReference if it has a
   * lower service ranking and greater if it has a higher service ranking.
   *
   * Otherwise, if this ServiceReference and the specified ServiceReference have
   * the same service ranking, this ServiceReference is less than the specified
   * ServiceReference if it has a higher service id and greater if it has a lower
   * service id.
   */
  def compareTo(reference: AnyRef): Int = {
    reference match {
      case ref: OSGiServiceReferenceImpl[_] => {
        val thisID = underlying.attributes(MC.ServiceID).asInstanceOf[Long]
        val thatID = ref.underlying.attributes(MC.ServiceID).asInstanceOf[Long]
        if (thisID == thatID) {
          0
        }
        else {
          val thisRank = underlying.attributes.getOrElse(MC.ServiceRanking, 0).asInstanceOf[Long]
          val thatRank = ref.underlying.attributes.getOrElse(MC.ServiceRanking, 0).asInstanceOf[Long]

          if (thisRank == thatRank) {
            if (thisID > thatID) {
              -1
            }
            else {
              1
            }
          }
          else if (thisRank > thatRank) {
            1
          }
          else {
            -1
          }
        }
      }
      case null => throw new NullPointerException()
      case other => throw new IllegalArgumentException(other.getClass.getName())
    }
  }

  /**
   * Tests if the bundle that registered the service referenced by this ServiceReference and the specified bundle use
   * the same source for the package of the specified class name.
   * This method performs the following checks:
   *
   * 1) Get the package name from the specified class name.
   * 2) For the bundle that registered the service referenced by this ServiceReference (registrant bundle); find the
   * source for the package. If no source is found then return true if the registrant bundle is equal to the specified
   * bundle; otherwise return false.
   * 3) If the package source of the registrant bundle is equal to the package source of the specified bundle then
   * return true; otherwise return false.
   *
   * @param bundle - The Bundle object to check.
   * @param className - The class name to check.
   *
   * @return true if the bundle which registered the service referenced by this ServiceReference and the specified
   * bundle use the same source for the package of the specified class name. Otherwise false is returned.
   *
   * @throws java.lang.IllegalArgumentException - If the specified Bundle was not created by the same framework instance
   * as this ServiceReference.
   */
  def isAssignableTo(bundle: osgi.Bundle, className: String) = {
    // TODO implement isAssignableTo
    throw new IllegalStateException("Not yet implemented")
  }

  /**
   * Returns the bundles that are using the service referenced by this ServiceReference object.
   * Specifically, this method returns the bundles whose usage count for that service is greater than zero.
   *
   * @return An array of bundles whose usage count for the service referenced by this ServiceReference object is greater
   * than zero; null if no bundles are currently using that service.
   */
  def getUsingBundles = {
    // TODO implement getUsingBundles
    throw new IllegalStateException("Not yet implemented")
  }

  /**
   * Returns the bundle that registered the service referenced by this ServiceReference object.
   * This method must return null when the service has been unregistered. This can be used to determine if the service
   * has been unregistered.

   * @return
   * The bundle that registered the service referenced by this ServiceReference object; null if that service has already
   * been unregistered.
   */
  def getBundle = {
    // TODO implement getBundle
    throw new IllegalStateException("Not yet implemented")
  }

  def getPropertyKeys: Array[String] = {
    underlying.attributes.keySet.map(key => {
      if (key == MC.ServiceID) {
        osgi.Constants.SERVICE_ID
      }
      else if (key == MC.ServiceRanking) {
        osgi.Constants.SERVICE_RANKING
      }
      else {
        key
      }
    }).toArray
  }

  def getProperty(key: String) = {
    // TODO make this more efficient by swapping key not mapping keys
    underlying.attributes.map(tuple2 => {
      if (tuple2._1 == MC.ServiceID) {
        OC.SERVICE_ID -> tuple2._2
      }
      else if (tuple2._1 == MC.ServiceRanking) {
        OC.SERVICE_RANKING -> tuple2._2
      }
      else {
        tuple2
      }
    }).apply(key).asInstanceOf[AnyRef]
  }

  /*
    VERSION of this method that allows recursive calls
  private val serviceRef = new HashMap[OSGiServiceReferenceImpl[S], Option[S]]() with SynchronizedMap[OSGiServiceReferenceImpl[S], Option[S]]

  private[osgi] def getService(): S = {
    def getServiceFromUnderlying = {
      try {
        underlying.service
      } catch {
        case e: RuntimeException => {
          // TODO log exception properly
          e.printStackTrace
        }
        None
      }
    }
    if (underlying.isAvailable) {
      val service = serviceRef.getOrElseUpdate(this, getServiceFromUnderlying match {
        case None => {
          serviceRef.remove(this)
          None
        }
        case s@Some(_) => s
      })

      service match {
        case Some(s) => {
          usage += 1
          s
        }
        case None => {
          null.asInstanceOf[S]
        }
      }
    }
    else {
      null.asInstanceOf[S]
    }

   */

  private class GetOnce[T](f: Function0[T]) extends Function0[T] {
    var value: T = _

    def apply(): T = synchronized {
      if (value == null) {
        value = f()
      }

      value
    }
  }

  private[osgi] def getService(): S = {
    val id = underlying.attributes(MC.ServiceID)

    if (OSGiServiceReferenceImpl.getting.add(id)) {
      // TODO use underlying service.id to test if already getting this service

      val found = if (underlying.isAvailable) {
        synchronized {
          val future = serviceFuture match {
            case Some(f) => {
              f
            }
            case None => {
              def get(): Box[S] = try {
                underlying.get()
              } catch {
                case e: RuntimeException => {
                  // TODO log exception properly
                  e.printStackTrace
                }
                Empty
              }
              val f = new GetOnce[Box[S]](get)
              serviceFuture = Some(f)
              f
            }
          }
          val result = future()
          result match {
            case Full(s) => {
              usage += 1
              s
            }
            case _ => {
              serviceFuture = None
              null.asInstanceOf[S]
            }
          }
        }
      }
      else {
        null.asInstanceOf[S]
      }

      OSGiServiceReferenceImpl.getting.remove(id)

      found
    }
    else {
      null.asInstanceOf[S]
    }
  }

  private[osgi] def ungetService(): Boolean = {
    if (underlying.isAvailable) {
      synchronized {
        if (usage > 0) {
          usage -= 1
          if (usage == 0) {
            serviceFuture match {
              case Some(ref) => {
                ref.value match {
                  case Full(s) => try {
                    underlying.unget(s)
                  }
                  catch {
                    case e: Exception => {
                      // TODO is this the right place to catch this?
                      // TODO log framework warning
                      e.printStackTrace()
                    }
                  }
                  case _ => // fine
                }
              }
              case None => // fine
            }
            serviceFuture = None
            false
          }
          else {
            true
          }
        }
        else {
          false
        }
      }
    }
    else {
      false
    }
  }
}

object OSGiServiceReferenceImpl {
  val getting = new HashSet[Any]() with SynchronizedSet[Any]
}
