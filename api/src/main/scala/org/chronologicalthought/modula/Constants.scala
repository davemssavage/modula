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

/**
 * @author David Savage
 */

object Constants {
  private val Base = "modula."

  object Directives {

    private val ResolutionBase = Base + "resolution."

    val ResolutionClosure = ResolutionBase + "closure"
    val ResolutionTransitiveClosure = "transitive"

    val ResolutionPolicy = ResolutionBase + "policy"
    val ResolutionMandatory = "mandatory"
    val ResolutionOptional = "optional"
    val ResolutionDynamic = "dynamic"

    val ResolutionCardinality = ResolutionBase + "cardinality"
    val ResolutionCardinalityMultiple = "multiple"
    val ResolutionCardinalitySingular = "single"

    private val ExtensionBase = Base + "extension."

    val ExtensionExtends = ExtensionBase + "extends"
    val ExtensionExtensible = ExtensionBase + "extensible"

    private val CapabilityBase = Base + "capability."

    val CapabilityUses = CapabilityBase + "uses"
    val CapabilityMandatory = CapabilityBase + "mandatory"

    private val RequirementBase = Base + "requirement."

    val RequirementFilter = RequirementBase + "filter"
  }

  private val ServiceBase = Base + "service."

  val ServiceID = ServiceBase + "id"
  val ServiceRanking = ServiceBase + "ranking"

  private val NamespaceBase = Base + "namespace."

  // TODO osgi has fragment namespace as well - decide if this is needed
  val ModuleNamespace = NamespaceBase + "module"
  val PackageNamespace = NamespaceBase + "package"
}
