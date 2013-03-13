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

import java.util.Comparator

/**
 * @author David Savage
 */

// TODO use this in ordering service references returned by findReferences
// ServiceComparators should be looked up by findServices which in turn allows them to be ordered
// ServiceComparators should only be visible in context that registers them
// may need new method on ModuleContext to register with filter that hides from
// other contexts?
// TODO should move this to hooks package?
trait ServiceComparator extends Comparator[ServiceReference[_]] {
  // TODO move these scopes to Hooks super trait
  /**
   * Service attribute to specify where the comparator is used
   */
  val Scope = "scope"

  /**
   * The service comparator is global
   */
  val Global = "global"

  /**
   * The service comparator is only used in the context that registers it
   */
  val Context = "context"

  /**
   * The default context of a comparator if non is specified on
   * registration
   */
  // TODO probably can't move default to Hooks super trait?
  val Default = Context
}
