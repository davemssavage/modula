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
import scala.Some

/**
 * @author David Savage
 */
private[impl] object WiredModules {
  val Empty = new WiredModules(Nil, Nil)
}

private[impl] case class WiredModules(dependents: Traversable[Module], extensions: Traversable[Module]) {
  lazy val all = dependents.toList ::: extensions.toList
}

private[impl] class ResolvableModule(id: Long, provider: ModuleProvider, ctx: ModuleContextImpl, frameworkContext: ModuleContext) extends ModuleProviderWrapper(id, provider, ctx) with StatefulModule {
  private var wiredModules: Box[WiredModules] = Empty

  def resolveModules(wire: Boolean): Box[WiredModules] = {
    val resolved = synchronized {
      wiredModules match {
        case f@Full(_) => f
        case Empty => {
          // TODO do resolve outside of synchronized block!!
          val modules = doResolveModules()

          if (modules.isDefined && wire)
            wiredModules = modules

          modules
        }
      }
    }

    resolved
  }

  override private[impl] def uninstall() {
    super.uninstall()

    // TODO this method is not yet implemented!!!
    // TODO need to do reference counting to figure out if module can be released from global wiring
    // TODO osgi spec says that if this module is in use (i.e. packages are exported) then the module
    // TODO should hang around in a zombie state till refresh packages is called, can this be improved upon?
    synchronized {
      wiredModules.map(_.all)
    }
  }


  override def toString = provider.name + ":" + provider.version

  private def doResolveModules(): Box[WiredModules] = {
    frameworkContext.withAnyFlatten(classOf[GlobalWiring]){
      wiring => {
        wiring.resolve(this).flatMap(resolution => {
          if (resolution.isEmpty) {
            Full(WiredModules.Empty)
          }
          else {
            new RichWiring(resolution).find(this).map(root => {
              val dependents = getDependentModules(root, resolution)
              val extensions = getExtensions(root)

              new WiredModules(dependents, extensions)
            })
          }
        })
      }
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