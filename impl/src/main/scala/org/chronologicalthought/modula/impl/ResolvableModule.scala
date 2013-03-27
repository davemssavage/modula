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
import scala.Some

/**
 * @author David Savage
 */

private[impl] case class WiredModules(dependents: Traversable[Module], extensions: Traversable[Module]) {
  lazy val all = dependents.toList ::: extensions.toList
}

private [impl] object ResolvableModule {
  private var globalWiring = RichWiring.empty
}

private[impl] class ResolvableModule(id: Long, provider: ModuleProvider, ctx: ModuleContextImpl, frameworkContext: ModuleContext) extends ModuleProviderWrapper(id, provider, ctx) with StatefulModule {
  private var wiredModules: Option[WiredModules] = None

  def resolveModules(wire: Boolean) = {
    val resolved = synchronized {
      wiredModules match {
        case Some(modules) => modules
        case None => {
          // TODO do resolve outside of synchronized block!!
          val modules = doResolveModules()

          if (wire)
            wiredModules = Some(modules)

          modules
        }
      }
    }

    debugWired(resolved)

    resolved
  }

  override private[impl] def uninstall() {
    super.uninstall()

    // TODO need to unwire this module from global wiring
    // TODO osgi spec says that if this module is in use (i.e. packages are exported) then the module
    // TODO should hang around in a zombie state till refresh packages is called, can this be improved upon?
    synchronized {
      wiredModules.map(_.all)
    }
  }


  override def toString = provider.name + ":" + provider.version

  private def doResolveModules(): WiredModules = {
    val resolved = frameworkContext.withAny(classOf[Resolver]) {
      resolver => {
        val environment = buildSnapshotEnvironment

        val resolution = resolver.resolve(environment, this.requirements)

        ResolvableModule.globalWiring = ResolvableModule.globalWiring + new RichWiring(resolution)

        debugResolution(resolution)

        if (resolution.isEmpty) {
          new WiredModules(Nil, Nil)
        }
        else {
          val root = if (resolution.contains(this)) {
            this
          } else {
            resolution.keys.find(p => p match {
              case c@CompositePart(parts) => parts.contains(this)
              case _ => false
            }) match {
              case Some(m) => m
              case None => throw new IllegalStateException("Failed to locate root in " + resolution)
            }
          }

          val dependents = getDependentModules(root, resolution)
          val extensions = getExtensions(root)

          new WiredModules(dependents, extensions)
        }
      }
    }

    // TODO define better exception for missing resolver
    resolved.openOr(throw new IllegalStateException("Expected resolver registered"))
  }

  private def buildSnapshotEnvironment = {
    val modulesSnapshot = frameworkContext.modules.values

    new Environment {
      def wiring = ResolvableModule.globalWiring.underlying

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

  private def getDependentModules(root: Part, resolution: Map[Part, Traversable[Wire]]): Traversable[Module] = {
    resolution.get(root) match {
      case Some(wires) => {
        wires.withFilter(wire => {
          wire.capability.part match {
            case module: Module => true
            case _ => false
          }
        }).map(_.capability.part.asInstanceOf[Module]).toSet
        //        wires.foreach(wire => {
        //          wire.capability.part match {
        //            case `provider` => // self reference
        //            case other => other match {
        //              case module: Module => {
        //
        //              }
        //              case other => // TODO use capability providers to download modules
        //            }
        //          }
        //        })
      }
      case None => Nil
    }
  }

  // finds other parts that have been merged with
  // this module as a composite part
  private def getExtensions(root: Part) = {
    root match {
      case CompositePart(parts) => {
        // TODO this is stupid, check first part - prevents recursion
        // this handles the case where the extendee is loading from the extender
        if (parts.head == this) {
          parts.withFilter(_ match {
            case m: Module => m != this
            case _ => false
          }).map(_.asInstanceOf[Module])
        }
        // this handles the case where the extender is loading from the extendee
        else if (parts.reverse.head == this) {
          parts.withFilter(_ match {
            case m: Module => m != this
            case _ => false
          }).map(_.asInstanceOf[Module])
        }
        else {
          Nil
        }
      }
      case _ => Nil
    }
  }

   // TODO tidy up debug code
  private def debugWired(wired: WiredModules) {
//    val buf = new StringBuilder()
//
//    buf.append("------------Wired----------\n")
//    buf.append(this)
//    buf.append("\n------------Dependents--------\n")
//    buf.append(wired.dependents.mkString("\n"))
//    buf.append("\n------------Extensions--------\n")
//    buf.append(wired.extensions.mkString("\n"))
//
//    println(buf.toString)
  }

  private def debugResolution(res: Map[Part, List[Wire]]) {
//    val buf = new StringBuilder()
//
//    buf.append("------------Resolution----------\n")
//    buf.append(this)
//    buf.append("\n------------Requirements--------\n")
//    for (r <- this.requirements) {
//      buf.append(r.namespace)
//      buf.append("(")
//      buf.append(r.attributes.getOrElse(r.namespace, "unknown"))
//      buf.append(")\n")
//    }
//    buf.append("------------Wiring----------\n")
//
//    for ((p, wires) <- res) {
//      buf.append(p)
//
//      buf.append(" { \n")
//      for (w <- wires) {
//        buf.append("  ")
//        buf.append(w.requirement.namespace)
//        buf.append("(")
//        buf.append(w.requirement.attributes.getOrElse(w.requirement.namespace, "unknown"))
//        buf.append(") : ")
//        buf.append(w.capability.part)
//        buf.append("\n")
//      }
//
//      buf.append("}\n")
//    }
//
//    println(buf.toString)
  }
}