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

import java.util.concurrent.locks.ReentrantReadWriteLock
import org.chronologicalthought.modula.{Module, ModuleContext}

/**
 * @author David Savage
 */

private[impl] trait FrameworkBase extends Module {

  private val internalContext: ModuleContext = newContext()

  def context = internalContext

  private val lock = new ReentrantReadWriteLock
  private var syncState = LockType.Unsynchronized

  private[impl] def lockState: LockType.LockType = {
    lock.readLock.lock()
    try {
      syncState
    }
    finally {
      lock.readLock.unlock()
    }
  }

  private[impl] def readLock[T](f: => T): T = {
    lock.readLock.lock()
    try {
      f
    }
    finally {
      lock.readLock.unlock()
    }
  }

  private[impl] def writeLock[T](f: => T): T = {
    lock.writeLock.lock()
    try {
      syncState match {
        case LockType.Synchronized => f
        case LockType.Unsynchronized => {
          syncState = LockType.Synchronized
          try {
            f
          }
          catch {
            // TODO this is not a sensible place to catch/log but convenient for now
            case e => e.printStackTrace()
            throw e
          }
          finally {
            syncState = LockType.Unsynchronized
          }
        }
      }
    }
    finally {
      lock.writeLock.unlock()
    }
  }

  protected def newContext(): ModuleContext
}

private[impl] object LockType extends Enumeration {
  type LockType = Value
  val Synchronized, Unsynchronized = Value
}

private[impl] object FrameworkAction extends Enumeration {
  type FrameworkAction = Value
  val Registered, Modified, Unregistering = Value
}
