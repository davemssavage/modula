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

/**
 * @author David Savage
 */

abstract class ModuleProviderWrapper(val id: Long, provider: ModuleProvider, ctx: ModuleContextImpl) extends Module {
  def start() = {
    provider.start()
  }

  def stop() = {
    // TODO why is unregisterAll here? shouldn't this be implicit in all stop operations?
    ctx.unregisterAll()
    provider.stop()
  }

  def resources(filter: (String) => Boolean, resolve: Boolean, local: Boolean) = {
    provider.resources(filter, resolve, local)
  }

  def attributes = provider.attributes

  def loadClass(name: String) = {
    provider.loadClass(name)
  }

  def version = provider.version

  def name = provider.name

  def context: ModuleContext = ctx

  def isProvidedBy(provider: ModuleProvider) = {
    this.provider == provider
  }

  def capabilities = provider.capabilities.map(cap => new Capability(cap.namespace, cap.attributes, cap.directives, this))

  def requirements = provider.requirements.map(req => new Requirement(req.namespace, req.attributes, req.directives, Some(this)))
}

