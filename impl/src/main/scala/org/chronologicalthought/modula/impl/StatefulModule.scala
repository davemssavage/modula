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

import org.chronologicalthought.modula.{ModuleState => State, ModuleLike, ModuleContext, Module, ServiceReference}
import State.ModuleState
import annotation.tailrec
import java.util.concurrent.TimeUnit
import concurrent.{Future, Promise}
import util.{Failure, Try, Success}

/**
 * @author David Savage
 */

// TODO fire module status events
trait StatefulModule extends Module {
  private var st = State.Installed

  def state = synchronized(st)

  def waitFor(target: ModuleState, timeout: Long, timeUnit: TimeUnit): Boolean = {
    require(target != null)
    require(timeout >= 0)

    def now = System.currentTimeMillis

    val breakable = timeout != 0
    var w = TimeUnit.MILLISECONDS.convert(timeout, timeUnit)

    val start = now

    synchronized {
      while (st != target && (!breakable || w > 0)) {
        wait(w)

        if (breakable) {
          w = w - (now - start)
        }
      }

      st == target
    }
  }

  abstract override def start() = {
    stateChange(State.Installed, State.Resolved, State.Starting)(State.Starting, State.Started) {
      super.start()
    }
  }

  abstract override def stop() = {
    stateChange(State.Started)(State.Stopping, State.Resolved, State.Installed) {
      super.stop()
    }
  }

  abstract override def context: ModuleContext = {
    new StatefulContext(super.context)
  }

  private def is(inState: ModuleState*) = {
    synchronized {
      inState.contains(st)
    }
  }

  private[impl] def uninstall() {
    val stop = synchronized {
      val started = st == State.Starting || st == State.Started
      st = State.Uninstalled
      started
    }

    if (stop) {
      super.stop()
    }
  }

  protected[this] def stateChange(start: ModuleState*)(transition: ModuleState, end: ModuleState, otherEnds: ModuleState*)(action: => Future[ModuleLike]): Future[ModuleLike] = {
    @tailrec
    def checkStart(): Try[Boolean] = {
      val result = if (st == State.Uninstalled) {
        Some(Failure(new IllegalStateException("Attempt to change state of uninstalled module")))
      }
      // already at end state skip action
      else if (st == end || otherEnds.contains(st)) {
        Some(Success(false))
      }
      // reached start state
      else if (start.contains(st)) {
        Some(Success(true))
      }
      else {
        None
      }

      result match {
        case Some(t) => {
          // try to wake up other threads that may be waiting for this state
          notify()
          t
        }
        case None => {
          // wait for notify
          wait(1000L)

          // do this again
          checkStart()
        }
      }
    }

    val attemptTransition = synchronized {
      checkStart().map(okToStart => {
        if (okToStart) {
          st = transition

          // wake up waiting threads
          notify()
        }

        okToStart
      })
    }

    try {
      attemptTransition match {
        case Success(true) => action // do transition
        case Success(false) => Future.successful(this) // already in correct state
        case Failure(e) => Future.failed(e) // pass on failure
      }
    }
    finally {
      synchronized {
        st = end
        notify()
      }
    }
  }

  class StatefulContext(underlying: ModuleContext) extends ModuleContext {
    def findReferences[T](clazz: Class[T], filter: (Map[String, Any]) => Boolean) = {
      if (is(State.Starting, State.Started, State.Stopping)) {
        underlying.findReferences(clazz, filter)
      }
      else {
        throw new IllegalStateException("Cannot read references whilst stopped")
      }
    }

    def findAnyReferences(clazz: String, filter: Map[String, Any] => Boolean): Traversable[ServiceReference[AnyRef]] = {
      if (is(State.Starting, State.Started, State.Stopping)) {
        underlying.findAnyReferences(clazz, filter)
      }
      else {
        throw new IllegalStateException("Cannot read references whilst stopped")
      }
    }

    def register[S](service: S, interfaces: Traversable[Class[_]], properties: Traversable[(String, Any)]) = {
      if (is(State.Starting, State.Started)) {
        underlying.register(service, interfaces, properties)
      }
      else {
        throw new IllegalStateException("Cannot register services whilst stopped")
      }
    }

    def registered: Traversable[ServiceReference[AnyRef]] = {
      if (is(State.Starting, State.Started, State.Stopping)) {
        underlying.registered
      }
      else {
        List.empty
      }
    }

    def inUse: Traversable[ServiceReference[AnyRef]] = {
      if (is(State.Starting, State.Started, State.Stopping)) {
        underlying.inUse
      }
      else {
        List.empty
      }
    }

    def property(key: String) = underlying.property(key)

    def module = underlying.module
  }

}
