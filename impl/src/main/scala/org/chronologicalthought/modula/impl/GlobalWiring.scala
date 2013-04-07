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

import org.chronologicalthought.modula._
import RichTypes._

/**
 * @author David Savage
 */
class GlobalWiring(context: ModuleContext) {
  private var globalWiring = RichWiring.empty

  def resolve(module: Module) = {
    synchronized {
      val environment = buildSnapshotEnvironment()

      context.withAnyFlatten(classOf[Resolver])(_.resolve(environment, module.requirements)).map {
        resolution => {
          globalWiring = globalWiring + resolution
          resolution
        }
      }
    }
  }

  private def buildSnapshotEnvironment() = {
    val modulesSnapshot = context.modules.values

    new Environment {
      def wiring = globalWiring.underlying

      def findProviders(requirements: Traversable[Requirement]) = {
        val capabilities = modulesSnapshot.flatMap(_.capabilities)
        for {
          req <- requirements
          cap <- capabilities
          if (req.matches(cap))
        } yield cap
        // TODO add capability provider service lookup
      }

      def findExtensions(capabilities: Traversable[Capability]) = {
        val requirements = modulesSnapshot.flatMap(_.requirements)
        for {
          req <- requirements
          cap <- capabilities
          if (req.matches(cap))
        } yield req
        // TODO add requirement provider service lookup?
        // what is a requirement provider??
      }

      def requirementFilter = None
    }
  }
}