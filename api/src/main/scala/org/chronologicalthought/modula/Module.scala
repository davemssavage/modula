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

import java.util.concurrent.TimeUnit
import ModuleState._

/**
 * @author David Savage
 */

trait Module extends ModuleLike with ModuleReference {
  def id: Long

  def state: ModuleState

  def waitFor(state: ModuleState) {
    waitFor(state, 0L, TimeUnit.NANOSECONDS)
  }

  def waitFor(state: ModuleState, timeout: Long): Boolean = waitFor(state, timeout, TimeUnit.MILLISECONDS)

  def waitFor(state: ModuleState, timeout: Long, timeUnit: TimeUnit): Boolean

  def isProvidedBy(provider: ModuleProvider): Boolean
}

