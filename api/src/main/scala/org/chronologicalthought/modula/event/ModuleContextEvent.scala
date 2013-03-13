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

package org.chronologicalthought.modula.event

import org.chronologicalthought.modula.ModuleState._
import org.chronologicalthought.modula.{Module, ServiceReference}

/**
 * @author David Savage
 */

sealed abstract class ModuleContextEvent

// service events

abstract class ServiceEvent extends ModuleContextEvent

case class ServiceRegisteredEvent(reference: ServiceReference[_],
                                  interfaces: Set[Class[_]],
                                  attributes: Map[String, Any]) extends ServiceEvent

case class ServiceModifiedEvent(reference: ServiceReference[_],
                                interfaces: Set[Class[_]],
                                attributes: Map[String, Any]) extends ServiceEvent

case class ServiceUnregisteringEvent(reference: ServiceReference[_],
                                     interfaces: Set[Class[_]],
                                     attributes: Map[String, Any]) extends ServiceEvent

// module events

abstract class ModuleEvent extends ModuleContextEvent

case class ModuleLifecycleEvent(module: Module, current: ModuleState,
                                prev: ModuleState) extends ModuleEvent

// TODO should make it possible for providers to file update events
case class ModuleUpdateEvent(module: Module, current: ModuleState, prev: ModuleState)
  extends ModuleEvent

// framework events

// TODO map framework events to org.osgi.framework.FrameworkEvent
case object FrameworkEvent extends ModuleContextEvent
