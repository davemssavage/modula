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

import ref.Reference

/**
 * Created by IntelliJ IDEA.
 * User: dave
 * Date: 28/06/2012
 * Time: 13:31
 * To change this template use File | Settings | File Templates.
 */

trait GarbageCollector {
  // TODO hooks could return a Future to get value from onCollect?

  def phantomHook[T <: AnyRef](value: T)(onCollect: => Unit)

  def phantomRefHook[T <: AnyRef](value: T)(onCollect: Reference[T] => Unit): Reference[T]

  def weakHook[T <: AnyRef](value: T)(onCollect: => Unit)

  def weakRefHook[T <: AnyRef](value: T)(onCollect: Reference[T] => Unit): Reference[T]

  def softHook[T <: AnyRef](value: T)(onCollect: => Unit)

  def softRefHook[T <: AnyRef](value: T)(onCollect: Reference[T] => Unit): Reference[T]
}
