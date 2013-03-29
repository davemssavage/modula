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

import java.net.URL
import org.chronologicalthought.modula._
import org.chronologicalthought.modula.event.ModuleContextListener._
import org.chronologicalthought.modula.event.{ModuleContextListener, ServiceRegisteredEvent, ServiceUnregisteringEvent}
import collection.mutable.{ArrayBuffer, HashMap}
import javax.swing.SwingConstants
import reflect.{ObjenesisProxyBuilder, GarbageCollectorImpl, StandardJavaProxyBuilder}
import org.chronologicalthought.modula.reflect.{GarbageCollector, ProxyBuilder}
import concurrent.Future

/**
 * @author David Savage
 */
object FrameworkFactoryImpl extends FrameworkFactory {
  def newFramework(config: Map[String, Any] = Map.empty): Framework = {
    new FrameworkImpl(config) with StatefulModule
  }
}

abstract class FrameworkImpl(props: Map[String, Any]) extends Framework with ServiceProvider {

  val id = 0L
  val name: String = "Framework Module"
  val version: Version = Version.Empty

  private var moduleCount: Long = 0
  private val selfRegistrations: ArrayBuffer[ServiceRegistration[_]] = new ArrayBuffer
  private val registeredProviders = new HashMap[ModuleProvider, ServiceRegistration[ResolvableModule]]
  private val zombieProviders = new HashMap[ModuleProvider, ServiceRegistration[ResolvableModule]]

  def loadClass(name: String): Box[Class[_]] = {
    try {
      Full(getClass.getClassLoader.loadClass(name))
    }
    catch {
      case cnfe: ClassNotFoundException => {
        new Failure("Unknown class %s".format(name), Full(cnfe), Empty)
      }
    }
  }

  def resources(filter: (String) => Boolean, resolve: Boolean, local: Boolean): Traversable[URL] = Nil

  // TODO should framework have configurable attributes?
  val attributes: Map[String, Any] = Map.empty

  // TODO version package exports
  // TODO this is unmanagable - need profiles...
  private val defaultCapabilities =
    newCapability(Constants.ModuleNamespace, Map(Constants.ModuleNamespace -> classOf[Framework].getPackage.getName), Map.empty) ::
      newCapability(Constants.PackageNamespace, Map(Constants.PackageNamespace -> classOf[Framework].getPackage.getName), Map.empty) ::
      newCapability(Constants.PackageNamespace, Map(Constants.PackageNamespace -> classOf[ScalaObject].getPackage.getName), Map.empty) ::
      newCapability(Constants.PackageNamespace, Map(Constants.PackageNamespace -> classOf[SwingConstants].getPackage.getName), Map.empty) ::
      Nil

  lazy val capabilities: List[Capability] = {
    property(FrameworkImplConstants.Capabilities) match {
      case Some(caps: Traversable[Capability]) => inheritCapabilities(caps.toList)
      // TODO some conversion from strings??
      case Some(o) => throw new IllegalStateException("Unexpected capability type" + o)
      case None => {
        val extra = property(FrameworkImplConstants.ExtraCapabilities) match {
          case Some(caps: Traversable[Capability]) => inheritCapabilities(caps.toList)
          // TODO some conversion from strings??
          case Some(o) => throw new IllegalStateException("Unexpected capability type" + o)
          case None => {
            Nil
          }
        }
        defaultCapabilities ::: extra
      }
    }
  }

  val requirements: List[Requirement] = {
    // Believe it can never make sense for framework to have external requirements
    Nil
  }

  def property(key: String): Option[Any] = {
    props.get(key) match {
      case s: Some[Any] => s
      case None => {
        // TODO  snapshot system properties at startup as equinox?
        val value = System.getProperties.get(key)
        if (value == null) {
          None
        }
        else {
          Some(value)
        }
      }
    }
  }

  def isProvidedBy(provider: ModuleProvider) = false

  def start(): Future[ModuleLike] = {
    // TODO use promise future on start/stop...
    selfRegistrations += context.register(this, classOf[Module], Map(Constants.ModuleID -> this.id))
    selfRegistrations += context.register(new ResolverImpl, classOf[Resolver])
    selfRegistrations += context.register(new GarbageCollectorImpl, classOf[GarbageCollector])
    selfRegistrations += context.register(new StandardJavaProxyBuilder, classOf[ProxyBuilder])
    selfRegistrations += context.register(new ObjenesisProxyBuilder, classOf[ProxyBuilder])
    selfRegistrations += context.register(new GlobalWiring(context))

    val moduleProviderListener = listener {
      case ServiceRegisteredEvent(reference, interfaces, _) => {
        withModuleProvider(reference, interfaces) {
          (module, provider) => handleProviderRegistered(module, provider)
        }
      }
      case ServiceUnregisteringEvent(reference, interfaces, _) => {
        withModuleProvider(reference, interfaces) {
          (module, provider) => handleProviderUnregistered(module, provider)
        }
      }
    }

    selfRegistrations += context.register(moduleProviderListener, classOf[ModuleContextListener])

    Future.successful(this)
  }

  def stop(): Future[ModuleLike] = {
    registeredProviders.values.foreach(_.service.stop())
    registeredProviders.clear()

    selfRegistrations.reverse.foreach(_.unregister())
    selfRegistrations.clear()

    Future.successful(this)
  }

  private def withModuleProvider(reference: ServiceReference[_], interfaces: Set[Class[_]])(f: (Module, ModuleProvider) => Unit) {
    if (interfaces.contains(classOf[ModuleProvider])) {
      val providerRef = reference.asInstanceOf[ServiceReference[ModuleProvider]]

      providerRef {
        f(reference.module, _)
      }
    }
  }

  private def handleProviderRegistered(module: Module, provider: ModuleProvider) {
    val moduleContext = new ModuleContextImpl(this)

    val resolvableModule = writeLock {
      moduleCount += 1
      new ResolvableModule(moduleCount, provider, moduleContext, context)
    }

    moduleContext.module_=(resolvableModule)

    // TODO define parent classloader of modules...
    val parentCL = props.get(FrameworkImplConstants.ParentClassLoader) match {
      case Some(cl: ClassLoader) => cl
      case Some(other) => throw new IllegalStateException("Invalid parent classloader " + other)
      case None => ClassLoader.getSystemClassLoader
    }

    provider.init(new ModuleProviderClassLoader(resolvableModule, parentCL))

    val reg = module.context.register(resolvableModule, classOf[Module], Map(Constants.ModuleID -> resolvableModule.id))

    writeLock {
      registeredProviders += provider -> reg
    }
  }

  private def handleProviderUnregistered(module: Module, provider: ModuleProvider) {
    val reg = readLock(registeredProviders.get(provider))

    reg match {
      case Some(registration) => {
        registration.service.uninstall()
        registration.unregister()
      }
      case None => // no action
    }

    // TODO osgi spec says that if bundle is wired then it should hang around in zombie state till refresh packages is called
    provider.destroy()

    // ok finally
    writeLock(registeredProviders.remove(provider))
  }

  protected def newContext(): ModuleContextImpl = {
    val ctx = new ModuleContextImpl(this)
    ctx.module_=(this)
    ctx
  }

  override def toString = name
}
