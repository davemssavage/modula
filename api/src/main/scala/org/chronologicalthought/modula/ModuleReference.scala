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

import collection.mutable.WeakHashMap

/**
 * @author David Savage
 */
trait ModuleReference {
  def context: ModuleContext
}

object ModuleReference {
  private val anyRefToModuleReference = new WeakHashMap[AnyRef, ModuleReference]()

  implicit def anyRefToModuleReference(ref: AnyRef): ModuleReference = {
    def findContext = {
      ref match {
        case ctx: ModuleContext => {
          new ModuleReference {
            def context = ctx
          }
        }
        case other => {
          ref.getClass.getClassLoader match {
            case mod: ModuleReference => mod
            case _ => throw new IllegalStateException("Unknown module reference") // TODO use framework context
          }
        }
      }
    }

    anyRefToModuleReference.getOrElseUpdate(ref, findContext)
  }
}