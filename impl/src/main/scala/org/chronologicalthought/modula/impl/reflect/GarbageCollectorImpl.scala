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

import org.chronologicalthought.modula.reflect.GarbageCollector
import ref._
import collection.mutable.HashSet

/**
 * @author David Savage
 */

class GarbageCollectorImpl extends GarbageCollector {
  private val referenceQueue = new ReferenceQueue
  private val references = new HashSet[Reference[_]]()

  trait RefTask[T <: AnyRef] {
    def onCollect()
  }

  class PhantomRefTask[T <: AnyRef](ref: T, oc: Reference[T] => Unit) extends PhantomReference(ref, referenceQueue) with RefTask[T] {
    def onCollect() = oc.apply(this)
  }

  class PhantomTask[T <: AnyRef](ref: T, oc: => Unit) extends PhantomReference(ref, referenceQueue) with RefTask[T] {
    def onCollect() = oc
  }

  class WeakRefTask[T <: AnyRef](ref: T, oc: Reference[T] => Unit) extends WeakReference(ref, referenceQueue) with RefTask[T] {
    def onCollect() = oc.apply(this)
  }

  class WeakTask[T <: AnyRef](ref: T, oc: => Unit) extends WeakReference(ref, referenceQueue) with RefTask[T] {
    def onCollect() = oc
  }

  class SoftRefTask[T <: AnyRef](ref: T, oc: Reference[T] => Unit) extends SoftReference(ref, referenceQueue) with RefTask[T] {
    def onCollect() = oc.apply(this)
  }

  class SoftTask[T <: AnyRef](ref: T, oc: => Unit) extends SoftReference(ref, referenceQueue) with RefTask[T] {
    def onCollect() = oc
  }

  def phantomHook[T <: AnyRef](value: T)(onCollect: => Unit) {
    val task = new PhantomTask(value, onCollect)
    references.synchronized {
      references += task
    }
  }

  def phantomRefHook[T <: AnyRef](value: T)(onCollect: Reference[T] => Unit): Reference[T] = {
    val task = new PhantomRefTask(value, onCollect)
    references.synchronized {
      references += task
    }
    task
  }

  def softHook[T <: AnyRef](value: T)(onCollect: => Unit) {
    val task = new SoftTask(value, onCollect)
    references.synchronized {
      references += task
    }
  }

  def softRefHook[T <: AnyRef](value: T)(onCollect: Reference[T] => Unit): Reference[T] = {
    val task = new SoftRefTask(value, onCollect)
    references.synchronized {
      references += task
    }
    task
  }

  def weakHook[T <: AnyRef](value: T)(onCollect: => Unit) {
    val task = new WeakTask(value, onCollect)
    references.synchronized {
      references += task
    }
  }

  def weakRefHook[T <: AnyRef](value: T)(onCollect: Reference[T] => Unit): Reference[T] = {
    val task = new WeakRefTask(value, onCollect)
    references.synchronized {
      references += task
    }
    task
  }

  private val garbageCollectQueue = new Thread {
    override def run() {
      while (true) {
        try {
          val r = referenceQueue.remove
          // explicitly don't match on anything else as something has gone wrong with the universe if
          // we get another type of ref out of here
          (r: @unchecked) match {
            case Some(task: RefTask[AnyRef]) => {
              task.onCollect()
              references.synchronized {
                references -= task
              }
            }
          }
        }
        catch {
          // TODO handle logging
          case t => t.printStackTrace()
        }
      }
    }
  }

  garbageCollectQueue.setDaemon(true)
  garbageCollectQueue.start()
}
