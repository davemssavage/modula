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

package org.chronologicalthought.modula.osgi

import java.io.File
import java.io.InputStream
import java.lang.String
import org.chronologicalthought.{modula => mod}
import mod.{event => modevt}
import org.osgi.framework._
import collection.JavaConversions
import java.util.{IdentityHashMap, Dictionary}
import org.chronologicalthought.modula.{Box, Full, Empty}

/**
 * @author David Savage
 */

class BundleContextImpl(bundle: Bundle, underlying: mod.ModuleContext) extends BundleContext {

  private val selfRegistrations = new IdentityHashMap[AnyRef, mod.ServiceRegistration[modevt.ModuleContextListener]]

  def getProperty(key: String): String = {
    underlying.property(key).map(_.toString).getOrElse(null)
  }

  // TODO extract utility method to separate class
  private def withFactoryOrElseThrow[T](f: (BundleFactory) => T): T = {
    val found = underlying.withAny(classOf[BundleFactory])(f(_))
    found.openOr(throw new IllegalStateException("Missing bundle factory"))
  }

  /**
   * Installs a bundle from the specified {@code InputStream} object.
   *
   * <p>
   * If the specified {@code InputStream} is {@code null}, the Framework must
   * create the {@code InputStream} from which to read the bundle by
   * interpreting, in an implementation dependent manner, the specified
   * {@code location}.
   *
   * <p>
   * The specified {@code location} identifier will be used as the identity of
   * the bundle. Every installed bundle is uniquely identified by its location
   * identifier which is typically in the form of a URL.
   *
   * <p>
   * The following steps are required to install a bundle:
   * <ol>
   * <li>If a bundle containing the same location identifier is already
   * installed, the {@code Bundle} object for that bundle is returned.
   *
   * <li>The bundle's content is read from the input stream. If this fails, a
   * {@link BundleException} is thrown.
   *
   * <li>The bundle's associated resources are allocated. The associated
   * resources minimally consist of a unique identifier and a persistent
   * storage area if the platform has file system support. If this step fails,
   * a {@code BundleException} is thrown.
   *
   * <li>The bundle's state is set to {@code INSTALLED}.
   *
   * <li>A bundle event of type {@link BundleEvent#INSTALLED} is fired.
   *
   * <li>The {@code Bundle} object for the newly or previously installed
   * bundle is returned.
   * </ol>
   *
   * <b>Postconditions, no exceptions thrown </b>
   * <ul>
   * <li>{@code getState()} in &#x007B; {@code INSTALLED}, {@code RESOLVED}
   * &#x007D;.
   * <li>Bundle has a unique ID.
   * </ul>
   * <b>Postconditions, when an exception is thrown </b>
   * <ul>
   * <li>Bundle is not installed. If there was an existing bundle for the
   * specified location, then that bundle must still be in the state it was
   * prior to calling this method.</li>
   * </ul>
   *
   * @param location The location identifier of the bundle to install.
   * @param input The {@code InputStream} object from which this bundle will
   *        be read or {@code null} to indicate the Framework must create the
   *        input stream from the specified location identifier. The input
   *        stream must always be closed when this method completes, even if
   *        an exception is thrown.
   * @return The {@code Bundle} object of the installed bundle.
   * @throws BundleException If the installation failed. BundleException types
   *         thrown by this method include: {@link BundleException#READ_ERROR}
   *         , {@link BundleException#DUPLICATE_BUNDLE_ERROR},
   * {@link BundleException#MANIFEST_ERROR}, and
   * {@link BundleException#REJECTED_BY_HOOK}.
   * @throws SecurityException If the caller does not have the appropriate
   * {@code AdminPermission[installed bundle,LIFECYCLE]}, and the Java
   *         Runtime Environment supports permissions.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   */
  def installBundle(location: String, input: InputStream): Bundle = {
    withFactoryOrElseThrow {
      factory => factory.createBundle(location, input)
    }
  }


  /**
   * Installs a bundle from the specified {@code location} identifier.
   *
   * <p>
   * This method performs the same function as calling
   * {@link #installBundle(String,InputStream)} with the specified
   * {@code location} identifier and a {@code null} InputStream.
   *
   * @param location The location identifier of the bundle to install.
   * @return The {@code Bundle} object of the installed bundle.
   * @throws BundleException If the installation failed. BundleException types
   *         thrown by this method include: {@link BundleException#READ_ERROR}
   *         , {@link BundleException#DUPLICATE_BUNDLE_ERROR},
   * {@link BundleException#MANIFEST_ERROR}, and
   * {@link BundleException#REJECTED_BY_HOOK}.
   * @throws SecurityException If the caller does not have the appropriate
   * {@code AdminPermission[installed bundle,LIFECYCLE]}, and the Java
   *         Runtime Environment supports permissions.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @see #installBundle(String, InputStream)
   */
  def installBundle(location: String): Bundle = {
    installBundle(location, null)
  }

  /**
   * Returns the {@code Bundle} object associated with this
   * {@code BundleContext}. This bundle is called the context bundle.
   *
   * @return The {@code Bundle} object associated with this
   * {@code BundleContext}.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   */
  def getBundle(): Bundle = {
    bundle
  }

  /**
   * Returns the bundle with the specified identifier.
   *
   * @param id The identifier of the bundle to retrieve.
   * @return A {@code Bundle} object or {@code null} if the identifier does
   *         not match any installed bundle.
   */
  def getBundle(id: Long): Bundle = {
    withFactoryOrElseThrow {
      factory => factory.getBundle(id).openOr(null)
    }
  }

  /**
   * Returns the bundle with the specified location.
   *
   * @param location The location of the bundle to retrieve.
   * @return A {@code Bundle} object or {@code null} if the location does not
   *         match any installed bundle.
   * @since 1.6
   */
  def getBundle(location: String): Bundle = {
    withFactoryOrElseThrow {
      factory => factory.getBundle(location).openOr(null)
    }
  }


  /**
   * Returns a list of all installed bundles.
   * <p>
   * This method returns a list of all bundles installed in the OSGi
   * environment at the time of the call to this method. However, since the
   * Framework is a very dynamic environment, bundles can be installed or
   * uninstalled at anytime.
   *
   * @return An array of {@code Bundle} objects, one object per installed
   *         bundle.
   */
  def getBundles(): Array[Bundle] = {
    withFactoryOrElseThrow {
      factory => factory.getBundles().toArray
    }
  }

  // TODO extract utility method to separate class
  private def addContextListener(ref: AnyRef)(f: PartialFunction[modevt.ModuleContextEvent, Unit]) {
    val listener = modevt.ModuleContextListener.listener(f)

    selfRegistrations.synchronized {
      val existing = selfRegistrations.get(ref)

      if (existing == null) {
        val reg = underlying.register(listener, classOf[modevt.ModuleContextListener])
        selfRegistrations.put(ref, reg)
      }
      else {
        existing.service_=(listener)
      }
    }
  }

  private def removeContextListener(ref: AnyRef) {
    selfRegistrations.synchronized {
      val old = selfRegistrations.remove(ref)
      if (old != null)
        old.unregister
    }
  }

  /**
   * Adds the specified {@code ServiceListener} object with the specified
   * {@code filter} to the context bundle's list of listeners. See
   * {@link Filter} for a description of the filter syntax.
   * {@code ServiceListener} objects are notified when a service has a
   * lifecycle state change.
   *
   * <p>
   * If the context bundle's list of listeners already contains a listener
   * {@code l} such that {@code (l= =listener)}, then this method replaces that
   * listener's filter (which may be {@code null}) with the specified one
   * (which may be {@code null}).
   *
   * <p>
   * The listener is called if the filter criteria is met. To filter based
   * upon the class of the service, the filter should reference the
   * {@link Constants#OBJECTCLASS} property. If {@code filter} is {@code null}
   * , all services are considered to match the filter.
   *
   * <p>
   * When using a {@code filter}, it is possible that the {@code ServiceEvent}
   * s for the complete lifecycle of a service will not be delivered to the
   * listener. For example, if the {@code filter} only matches when the
   * property {@code x} has the value {@code 1}, the listener will not be
   * called if the service is registered with the property {@code x} not set
   * to the value {@code 1}. Subsequently, when the service is modified
   * setting property {@code x} to the value {@code 1}, the filter will match
   * and the listener will be called with a {@code ServiceEvent} of type
   * {@code MODIFIED}. Thus, the listener will not be called with a
   * {@code ServiceEvent} of type {@code REGISTERED}.
   *
   * <p>
   * If the Java Runtime Environment supports permissions, the
   * {@code ServiceListener} object will be notified of a service event only
   * if the bundle that is registering it has the {@code ServicePermission} to
   * get the service using at least one of the named classes the service was
   * registered under.
   *
   * @param listener The {@code ServiceListener} object to be added.
   * @param filter The filter criteria.
   * @throws InvalidSyntaxException If {@code filter} contains an invalid
   *         filter string that cannot be parsed.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @see ServiceEvent
   * @see ServiceListener
   * @see ServicePermission
   */
  def addServiceListener(listener: ServiceListener, filter: String): Unit = {
    val ldap = if (filter == null) mod.LDAPExpr.True else mod.LDAPExpr(filter)

    addContextListener(listener) {
      case modevt.ServiceRegisteredEvent(reference, interfaces, attributes) => {
        if (ldap(attributes)) {
          val event = new ServiceEvent(ServiceEvent.REGISTERED, new OSGiServiceReferenceImpl(reference))
          listener.serviceChanged(event)
        }
      }
      case modevt.ServiceModifiedEvent(reference, interfaces, attributes) => {
        if (ldap(attributes)) {
          val event = new ServiceEvent(ServiceEvent.MODIFIED, new OSGiServiceReferenceImpl(reference))
          listener.serviceChanged(event)
        }
        // TODO doesn't handle last known ref ldap causing removal/add
      }
      case modevt.ServiceUnregisteringEvent(reference, interfaces, attributes) => {
        if (ldap(attributes)) {
          val event = new ServiceEvent(ServiceEvent.UNREGISTERING, new OSGiServiceReferenceImpl(reference))
          listener.serviceChanged(event)
        }
      }
    }
  }

  /**
   * Adds the specified {@code ServiceListener} object to the context bundle's
   * list of listeners.
   *
   * <p>
   * This method is the same as calling
   * {@code BundleContext.addServiceListener(ServiceListener listener,
   * String filter)} with {@code filter} set to {@code null}.
   *
   * @param listener The {@code ServiceListener} object to be added.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @see #addServiceListener(ServiceListener, String)
   */
  def addServiceListener(listener: ServiceListener): Unit = {
    addServiceListener(listener, null)
  }

  /**
   * Removes the specified {@code ServiceListener} object from the context
   * bundle's list of listeners.
   *
   * <p>
   * If {@code listener} is not contained in this context bundle's list of
   * listeners, this method does nothing.
   *
   * @param listener The {@code ServiceListener} to be removed.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   */
  def removeServiceListener(listener: ServiceListener) = {
    removeContextListener(listener)
  }

  /**
   * Adds the specified {@code BundleListener} object to the context bundle's
   * list of listeners if not already present. BundleListener objects are
   * notified when a bundle has a lifecycle state change.
   *
   * <p>
   * If the context bundle's list of listeners already contains a listener
   * {@code l} such that {@code (l= =listener)}, this method does nothing.
   *
   * @param listener The {@code BundleListener} to be added.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @throws SecurityException If listener is a
   * {@code SynchronousBundleListener} and the caller does not have
   *         the appropriate {@code AdminPermission[context bundle,LISTENER]},
   *         and the Java Runtime Environment supports permissions.
   * @see BundleEvent
   * @see BundleListener
   */
  def addBundleListener(listener: BundleListener) {
    addContextListener(listener) {
      case modevt.ModuleLifecycleEvent(module, currentState, previousState) => {
        // TODO implement context addBundleListener + security check
      }
      case modevt.ModuleUpdateEvent(module, currentState, previousState) => {
        // TODO implement context addBundleListener + security check
      }
    }
  }

  /**
   * Removes the specified {@code BundleListener} object from the context
   * bundle's list of listeners.
   *
   * <p>
   * If {@code listener} is not contained in the context bundle's list of
   * listeners, this method does nothing.
   *
   * @param listener The {@code BundleListener} object to be removed.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @throws SecurityException If listener is a
   * {@code SynchronousBundleListener} and the caller does not have
   *         the appropriate {@code AdminPermission[context bundle,LISTENER]},
   *         and the Java Runtime Environment supports permissions.
   */
  def removeBundleListener(listener: BundleListener) = {
    removeContextListener(listener)
  }

  /**
   * Adds the specified {@code FrameworkListener} object to the context
   * bundle's list of listeners if not already present. FrameworkListeners are
   * notified of general Framework events.
   *
   * <p>
   * If the context bundle's list of listeners already contains a listener
   * {@code l} such that {@code (l= =listener)}, this method does nothing.
   *
   * @param listener The {@code FrameworkListener} object to be added.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @see FrameworkEvent
   * @see FrameworkListener
   */
  def addFrameworkListener(listener: FrameworkListener) = {
    // TODO implement context addFrameworkListener
    throw new IllegalStateException("Not yet implemented")
  }

  /**
   * Removes the specified {@code FrameworkListener} object from the context
   * bundle's list of listeners.
   *
   * <p>
   * If {@code listener} is not contained in the context bundle's list of
   * listeners, this method does nothing.
   *
   * @param listener The {@code FrameworkListener} object to be removed.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   */
  def removeFrameworkListener(listener: FrameworkListener) = {
    removeContextListener(listener)
  }

  /**
   * Registers the specified service object with the specified properties
   * under the specified class names into the Framework. A
   * {@code ServiceRegistration} object is returned. The
   * {@code ServiceRegistration} object is for the private use of the bundle
   * registering the service and should not be shared with other bundles. The
   * registering bundle is defined to be the context bundle. Other bundles can
   * locate the service by using either the {@link #getServiceReferences} or
   * {@link #getServiceReference} method.
   *
   * <p>
   * A bundle can register a service object that implements the
   * {@link ServiceFactory} interface to have more flexibility in providing
   * service objects to other bundles.
   *
   * <p>
   * The following steps are required to register a service:
   * <ol>
   * <li>If {@code service} is not a {@code ServiceFactory}, an
   * {@code IllegalArgumentException} is thrown if {@code service} is not an
   * {@code instanceof} all the specified class names.
   * <li>The Framework adds the following service properties to the service
   * properties from the specified {@code Dictionary} (which may be
   * {@code null}): <br/>
   * A property named {@link Constants#SERVICE_ID} identifying the
   * registration number of the service <br/>
   * A property named {@link Constants#OBJECTCLASS} containing all the
   * specified classes. <br/>
   * Properties with these names in the specified {@code Dictionary} will be
   * ignored.
   * <li>The service is added to the Framework service registry and may now be
   * used by other bundles.
   * <li>A service event of type {@link ServiceEvent#REGISTERED} is fired.
   * <li>A {@code ServiceRegistration} object for this registration is
   * returned.
   * </ol>
   *
   * @param clazzes The class names under which the service can be located.
   *        The class names in this array will be stored in the service's
   *        properties under the key {@link Constants#OBJECTCLASS}.
   * @param service The service object or a {@code ServiceFactory} object.
   * @param properties The properties for this service. The keys in the
   *        properties object must all be {@code String} objects. See
   * {@link Constants} for a list of standard service property keys.
   *        Changes should not be made to this object after calling this
   *        method. To update the service's properties the
   * {@link ServiceRegistration#setProperties} method must be called.
   *        The set of properties may be {@code null} if the service has no
   *        properties.
   * @return A {@code ServiceRegistration} object for use by the bundle
   *         registering the service to update the service's properties or to
   *         unregister the service.
   * @throws IllegalArgumentException If one of the following is true:
   *         <ul>
   *         <li>{@code service} is {@code null}. <li>{@code service} is not a
   * {@code ServiceFactory} object and is not an instance of all the
   *         named classes in {@code clazzes}. <li> {@code properties}
   *         contains case variants of the same key name.
   *         </ul>
   * @throws SecurityException If the caller does not have the
   * {@code ServicePermission} to register the service for all the
   *         named classes and the Java Runtime Environment supports
   *         permissions.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @see ServiceRegistration
   * @see ServiceFactory
   */
  def registerService(clazzes: Array[String], service: Object, properties: Dictionary[String, _]): ServiceRegistration[_] = {
    val classesList = clazzes.toList

    service match {
      case null => throw new IllegalArgumentException("Service cannot be null")
      case factory: ServiceFactory[_] => {
        // TODO not sure this is a valid way to load classes
        // what if service is trying to register a factory for an object
        // not in it's classloader?
        val osgiProperties = userProperties(properties) + (Constants.OBJECTCLASS -> clazzes)
        doRegisterServiceFactory(classesList, factory, osgiProperties)
      }
      case other => {
        val osgiProperties = userProperties(properties) + (Constants.OBJECTCLASS -> clazzes)
        doRegisterService(classesList, service, osgiProperties)
      }
    }
  }

  private def doRegisterService(clazzes: List[String], service: Object, properties: Map[String, Any]): ServiceRegistration[_] = {
    // TODO can this be made tail recursive?
    def searchDeclaredClasses(clazz: Class[_]): List[Class[_]] = {
      val sp = clazz.getSuperclass
      val interfaces: List[Class[_]] = clazz.getInterfaces.toList
      val allInterfaces = interfaces.foldLeft(List.empty[Class[_]])((list, c) => list ::: searchDeclaredClasses(c))
      val clazzAndInterfaces = clazz :: allInterfaces
      if (sp == null) {
        clazzAndInterfaces
      }
      else {
        clazzAndInterfaces ::: searchDeclaredClasses(sp)
      }
    }

    val declaredClasses = searchDeclaredClasses(service.getClass)
    val registeredInterfaces = declaredClasses.filter(c => clazzes.contains(c.getName))

    if (clazzes.size != registeredInterfaces.size) {
      throw new IllegalArgumentException("Service does not implement all declared interfaces")
    }

    val reg = underlying.register(service, registeredInterfaces, properties)
    new OSGiServiceRegistrationImpl(reg, underlying)
  }

  private def registerServiceFactory[S](clazzes: List[Class[_]], factory: ServiceFactory[S], properties: Map[String, Any]): ServiceRegistration[S] = {
    def castRegistration[T](reg: ServiceRegistration[_]): ServiceRegistration[T] = {
      reg.asInstanceOf[ServiceRegistration[T]]
    }

    def castService[T](service: Any): T = {
      service.asInstanceOf[T]
    }

    def toBundle(module: mod.Module): Box[Bundle] = {
      underlying.withAny(classOf[BundleFactory])(_.getBundle(module.id)).openOr(Empty)
    }

    val modFactory = new mod.ServiceFactory[Any] {
      def getService(module: mod.Module, registration: mod.ServiceRegistration[Any]) = {
        toBundle(module).flatMap(bundle => {
          val reg = new OSGiServiceRegistrationImpl[Any](registration, underlying)
          val s = factory.getService(bundle, castRegistration(reg))
          Box.legacyNullTest(s)
        })
      }

      def ungetService(module: mod.Module, registration: mod.ServiceRegistration[Any], service: Any) = {
        if (toBundle(module).map(bundle => {
          val reg = new OSGiServiceRegistrationImpl[Any](registration, underlying)
          factory.ungetService(bundle, castRegistration(reg), castService(service))
        }).isEmpty) {
          // TODO not quite sure what to do here...
          throw new IllegalStateException("Invalid bundle " + module)
        }
      }
    }

    val reg = underlying.register(modFactory, clazzes, properties)
    new OSGiServiceRegistrationImpl(reg, underlying).asInstanceOf[ServiceRegistration[S]]
  }

  private def doRegisterServiceFactory[S](clazzes: List[String], factory: ServiceFactory[S], properties: Map[String, Any]): ServiceRegistration[S] = {
    val found = clazzes.map(clazz => underlying.module.loadClass(clazz))
    val inferredInterfaces = found.filterNot(_.isEmpty).map(_.openOrThrowException("Filter failed to remove empty boxes"))
    registerServiceFactory(inferredInterfaces, factory, properties)
  }

  /**
   * Registers the specified service object with the specified properties
   * under the specified class name with the Framework.
   *
   * <p>
   * This method is otherwise identical to
   * {@link #registerService(String[], Object, Dictionary)} and is provided as
   * a convenience when {@code service} will only be registered under a single
   * class name. Note that even in this case the value of the service's
   * {@link Constants#OBJECTCLASS} property will be an array of string, rather
   * than just a single string.
   *
   * @param clazz The class name under which the service can be located.
   * @param service The service object or a {@code ServiceFactory} object.
   * @param properties The properties for this service.
   * @return A {@code ServiceRegistration} object for use by the bundle
   *         registering the service to update the service's properties or to
   *         unregister the service.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @see #registerService(String[], Object, Dictionary)
   */
  //  def registerService(clazz: String, service: Object, properties: Dictionary[String,_]):ServiceRegistration[_] = {
  //    throw new IllegalStateException("Not yet implemented")
  //  }
  //
  def registerService(clazz: String, service: AnyRef, properties: Dictionary[String, _]) = {
    registerService(Array(clazz), service, properties)
  }


  /**
   * Registers the specified service object with the specified properties
   * under the specified class name with the Framework.
   *
   * <p>
   * This method is otherwise identical to
   * {@link #registerService(String[], Object, Dictionary)} and is provided as
   * a convenience when {@code service} will only be registered under a single
   * class name. Note that even in this case the value of the service's
   * {@link Constants#OBJECTCLASS} property will be an array of string, rather
   * than just a single string.
   *
   * @param <S> Type of Service.
   * @param clazz The class name under which the service can be located.
   * @param service The service object or a {@code ServiceFactory} object.
   * @param properties The properties for this service.
   * @return A {@code ServiceRegistration} object for use by the bundle
   *         registering the service to update the service's properties or to
   *         unregister the service.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @see #registerService(String[], Object, Dictionary)
   * @since 1.6
   */
  def registerService[S](clazz: Class[S], service: S, properties: Dictionary[String, _]): ServiceRegistration[S] = {
    // TODO tidy up copy paste from registerService(String[])

    service match {
      case null => throw new IllegalArgumentException("Service cannot be null")
      case factory: ServiceFactory[S] => {
        val osgiProperties = userProperties(properties) + (Constants.OBJECTCLASS -> Array(clazz.getName))
        registerServiceFactory(List(clazz), factory, osgiProperties)
      }
      case other => {
        if (!clazz.isInstance(service)) {
          throw new IllegalArgumentException("Service does not implement declared interface")
        }

        val osgiProperties = userProperties(properties) + (Constants.OBJECTCLASS -> Array(clazz.getName))
        val reg = underlying.register(service, clazz, osgiProperties)
        new OSGiServiceRegistrationImpl(reg, underlying)
      }
    }
  }

  private def userProperties(properties: Dictionary[String, _]): Map[String, Any] = {
    if (properties == null) {
      Map.empty
    }
    else {
      val dictionary = properties.asInstanceOf[Dictionary[String, Any]]
      JavaConversions.dictionaryAsScalaMap(dictionary).toMap
    }
  }

  /**
   * Returns an array of {@code ServiceReference} objects. The returned array
   * of {@code ServiceReference} objects contains services that were
   * registered under the specified class, match the specified filter
   * expression, and the packages for the class names under which the services
   * were registered match the context bundle's packages as defined in
   * {@link ServiceReference#isAssignableTo(Bundle, String)}.
   *
   * <p>
   * The list is valid at the time of the call to this method. However since
   * the Framework is a very dynamic environment, services can be modified or
   * unregistered at any time.
   *
   * <p>
   * The specified {@code filter} expression is used to select the registered
   * services whose service properties contain keys and values which satisfy
   * the filter expression. See {@link Filter} for a description of the filter
   * syntax. If the specified {@code filter} is {@code null}, all registered
   * services are considered to match the filter. If the specified
   * {@code filter} expression cannot be parsed, an
   * {@link InvalidSyntaxException} will be thrown with a human readable
   * message where the filter became unparsable.
   *
   * <p>
   * The result is an array of {@code ServiceReference} objects for all
   * services that meet all of the following conditions:
   * <ul>
   * <li>If the specified class name, {@code clazz}, is not {@code null}, the
   * service must have been registered with the specified class name. The
   * complete list of class names with which a service was registered is
   * available from the service's {@link Constants#OBJECTCLASS objectClass}
   * property.
   * <li>If the specified {@code filter} is not {@code null}, the filter
   * expression must match the service.
   * <li>If the Java Runtime Environment supports permissions, the caller must
   * have {@code ServicePermission} with the {@code GET} action for at least
   * one of the class names under which the service was registered.
   * <li>For each class name with which the service was registered, calling
   * {@link ServiceReference#isAssignableTo(Bundle, String)} with the context
   * bundle and the class name on the service's {@code ServiceReference}
   * object must return {@code true}
   * </ul>
   *
   * @param clazz The class name with which the service was registered or
   * {@code null} for all services.
   * @param filter The filter expression or {@code null} for all services.
   * @return An array of {@code ServiceReference} objects or {@code null} if
   *         no services are registered which satisfy the search.
   * @throws InvalidSyntaxException If the specified {@code filter} contains
   *         an invalid filter expression that cannot be parsed.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   */
  def getServiceReferences(clazz: String, filter: String): Array[ServiceReference[_]] = {
    // TODO map service attribute values from scala types to arrays - Constants.ObjectClass for example
    import mod.LDAPExpr
    import mod.LDAPExpr._

    val filterLDAP = if (filter == null) LDAPExpr.True else LDAPExpr(filter)

    val query = if (clazz == null) {
      filterLDAP
    }
    else {
      val findByName = expr(Constants.OBJECTCLASS, `=`, clazz)
      and(findByName, filterLDAP)
    }

    val refs = underlying.findReferences(null.asInstanceOf[Class[_]], query)
    if (refs.isEmpty) {
      null
    } else {
      refs.toArray.map(new OSGiServiceReferenceImpl(_))
    }
  }

  /**
   * Returns an array of {@code ServiceReference} objects. The returned array
   * of {@code ServiceReference} objects contains services that were
   * registered under the specified class and match the specified filter
   * expression.
   *
   * <p>
   * The list is valid at the time of the call to this method. However since
   * the Framework is a very dynamic environment, services can be modified or
   * unregistered at any time.
   *
   * <p>
   * The specified {@code filter} expression is used to select the registered
   * services whose service properties contain keys and values which satisfy
   * the filter expression. See {@link Filter} for a description of the filter
   * syntax. If the specified {@code filter} is {@code null}, all registered
   * services are considered to match the filter. If the specified
   * {@code filter} expression cannot be parsed, an
   * {@link InvalidSyntaxException} will be thrown with a human readable
   * message where the filter became unparsable.
   *
   * <p>
   * The result is an array of {@code ServiceReference} objects for all
   * services that meet all of the following conditions:
   * <ul>
   * <li>If the specified class name, {@code clazz}, is not {@code null}, the
   * service must have been registered with the specified class name. The
   * complete list of class names with which a service was registered is
   * available from the service's {@link Constants#OBJECTCLASS objectClass}
   * property.
   * <li>If the specified {@code filter} is not {@code null}, the filter
   * expression must match the service.
   * <li>If the Java Runtime Environment supports permissions, the caller must
   * have {@code ServicePermission} with the {@code GET} action for at least
   * one of the class names under which the service was registered.
   * </ul>
   *
   * @param clazz The class name with which the service was registered or
   * {@code null} for all services.
   * @param filter The filter expression or {@code null} for all services.
   * @return An array of {@code ServiceReference} objects or {@code null} if
   *         no services are registered which satisfy the search.
   * @throws InvalidSyntaxException If the specified {@code filter} contains
   *         an invalid filter expression that cannot be parsed.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @since 1.3
   */
  def getAllServiceReferences(clazz: String, filter: String): Array[ServiceReference[_]] = {
    // TODO this is a direct copy from getServiceReferences above - need to implement differences...
    // TODO map service attribute values from scala types to arrays - Constants.ObjectClass for example
    import mod.LDAPExpr
    import mod.LDAPExpr._

    val filterLDAP = if (filter == null) LDAPExpr.True else LDAPExpr(filter)

    val query = if (clazz == null) {
      filterLDAP
    }
    else {
      val findByName = expr(Constants.OBJECTCLASS, `=`, clazz)
      and(findByName, filterLDAP)
    }

    val refs = underlying.findReferences(null.asInstanceOf[Class[_]], query)
    if (refs.isEmpty) {
      null
    } else {
      refs.toArray.map(new OSGiServiceReferenceImpl(_))
    }
  }

  /**
   * Returns a {@code ServiceReference} object for a service that implements
   * and was registered under the specified class.
   *
   * <p>
   * The returned {@code ServiceReference} object is valid at the time of the
   * call to this method. However as the Framework is a very dynamic
   * environment, services can be modified or unregistered at any time.
   *
   * <p>
   * This method is the same as calling
   * {@link #getServiceReferences(String, String)} with a {@code null} filter
   * expression and then finding the reference with the highest priority. It
   * is provided as a convenience for when the caller is interested in any
   * service that implements the specified class.
   * <p>
   * If multiple such services exist, the service with the highest priority is
   * selected. This priority is defined as the service reference with the
   * highest ranking (as specified in its {@link Constants#SERVICE_RANKING}
   * property) is returned.
   * <p>
   * If there is a tie in ranking, the service with the lowest service ID (as
   * specified in its {@link Constants#SERVICE_ID} property); that is, the
   * service that was registered first is returned.
   *
   * @param clazz The class name with which the service was registered.
   * @return A {@code ServiceReference} object, or {@code null} if no services
   *         are registered which implement the named class.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @see #getServiceReferences(String, String)
   */
  def getServiceReference(clazz: String): ServiceReference[_] = {
    val findByName = mod.LDAPExpr.expr(Constants.OBJECTCLASS, mod.LDAPExpr.`=`, clazz)
    underlying.anyReference(null.asInstanceOf[Class[_]], findByName).map(new OSGiServiceReferenceImpl(_)).openOr(null)
  }

  /**
   * Returns a {@code ServiceReference} object for a service that implements
   * and was registered under the specified class.
   *
   * <p>
   * The returned {@code ServiceReference} object is valid at the time of the
   * call to this method. However as the Framework is a very dynamic
   * environment, services can be modified or unregistered at any time.
   *
   * <p>
   * This method is the same as calling
   * {@link #getServiceReferences(Class, String)} with a {@code null} filter
   * expression. It is provided as a convenience for when the caller is
   * interested in any service that implements the specified class.
   * <p>
   * If multiple such services exist, the service with the highest ranking (as
   * specified in its {@link Constants#SERVICE_RANKING} property) is returned.
   * <p>
   * If there is a tie in ranking, the service with the lowest service ID (as
   * specified in its {@link Constants#SERVICE_ID} property); that is, the
   * service that was registered first is returned.
   *
   * @param <S> Type of Service.
   * @param clazz The class name with which the service was registered.
   * @return A {@code ServiceReference} object, or {@code null} if no services
   *         are registered which implement the named class.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @see #getServiceReferences(Class, String)
   * @since 1.6
   */
  def getServiceReference[S](clazz: Class[S]): ServiceReference[S] = {
    underlying.anyReference(clazz).map(new OSGiServiceReferenceImpl(_)).openOr(null)
  }

  /**
   * Returns a collection of {@code ServiceReference} objects. The returned
   * collection of {@code ServiceReference} objects contains services that
   * were registered under the specified class, match the specified filter
   * expression, and the packages for the class names under which the services
   * were registered match the context bundle's packages as defined in
   * {@link ServiceReference#isAssignableTo(Bundle, String)}.
   *
   * <p>
   * The collection is valid at the time of the call to this method. However
   * since the Framework is a very dynamic environment, services can be
   * modified or unregistered at any time.
   *
   * <p>
   * The specified {@code filter} expression is used to select the registered
   * services whose service properties contain keys and values which satisfy
   * the filter expression. See {@link Filter} for a description of the filter
   * syntax. If the specified {@code filter} is {@code null}, all registered
   * services are considered to match the filter. If the specified
   * {@code filter} expression cannot be parsed, an
   * {@link InvalidSyntaxException} will be thrown with a human readable
   * message where the filter became unparsable.
   *
   * <p>
   * The result is a collection of {@code ServiceReference} objects for all
   * services that meet all of the following conditions:
   * <ul>
   * <li>If the specified class name, {@code clazz}, is not {@code null}, the
   * service must have been registered with the specified class name. The
   * complete list of class names with which a service was registered is
   * available from the service's {@link Constants#OBJECTCLASS objectClass}
   * property.
   * <li>If the specified {@code filter} is not {@code null}, the filter
   * expression must match the service.
   * <li>If the Java Runtime Environment supports permissions, the caller must
   * have {@code ServicePermission} with the {@code GET} action for at least
   * one of the class names under which the service was registered.
   * <li>For each class name with which the service was registered, calling
   * {@link ServiceReference#isAssignableTo(Bundle, String)} with the context
   * bundle and the class name on the service's {@code ServiceReference}
   * object must return {@code true}
   * </ul>
   *
   * @param <S> Type of Service
   * @param clazz The class name with which the service was registered. Must
   *        not be {@code null}.
   * @param filter The filter expression or {@code null} for all services.
   * @return A collection of {@code ServiceReference} objects. May be empty if
   *         no services are registered which satisfy the search.
   * @throws InvalidSyntaxException If the specified {@code filter} contains
   *         an invalid filter expression that cannot be parsed.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @since 1.6
   */
  def getServiceReferences[S](clazz: Class[S], filter: String): java.util.Collection[ServiceReference[S]] = {
    val refs = underlying.findReferences(clazz, mod.LDAPExpr(filter))
    JavaConversions.asJavaCollection(refs.toIterable.map(new OSGiServiceReferenceImpl(_)))
  }

  /**
   * Returns the service object referenced by the specified
   * {@code ServiceReference} object.
   * <p>
   * A bundle's use of a service is tracked by the bundle's use count of that
   * service. Each time a service's service object is returned by
   * {@link #getService(ServiceReference)} the context bundle's use count for
   * that service is incremented by one. Each time the service is released by
   * {@link #ungetService(ServiceReference)} the context bundle's use count
   * for that service is decremented by one.
   * <p>
   * When a bundle's use count for a service drops to zero, the bundle should
   * no longer use that service.
   *
   * <p>
   * This method will always return {@code null} when the service associated
   * with this {@code reference} has been unregistered.
   *
   * <p>
   * The following steps are required to get the service object:
   * <ol>
   * <li>If the service has been unregistered, {@code null} is returned.
   * <li>If the context bundle's use count for the service is currently zero
   * and the service was registered with an object implementing the
   * {@code ServiceFactory} interface, the
   * {@link ServiceFactory#getService(Bundle, ServiceRegistration)} method is
   * called to create a service object for the context bundle. If the service
   * object returned by the {@code ServiceFactory} object is {@code null}, not
   * an {@code instanceof} all the classes named when the service was
   * registered or the {@code ServiceFactory} object throws an exception or
   * will be recursively called for the context bundle, {@code null} is
   * returned and a Framework event of type {@link FrameworkEvent#ERROR}
   * containing a {@link ServiceException} describing the error is fired. <br>
   * This service object is cached by the Framework. While the context
   * bundle's use count for the service is greater than zero, subsequent calls
   * to get the services's service object for the context bundle will return
   * the cached service object.
   * <li>The context bundle's use count for this service is incremented by
   * one.
   * <li>The service object for the service is returned.
   * </ol>
   *
   * @param <S> Type of Service.
   * @param reference A reference to the service.
   * @return A service object for the service associated with
   * {@code reference} or {@code null} if the service is not
   *         registered, the service object returned by a
   * {@code ServiceFactory} does not implement the classes under which
   *         it was registered or the {@code ServiceFactory} threw an
   *         exception.
   * @throws SecurityException If the caller does not have the
   * {@code ServicePermission} to get the service using at least one
   *         of the named classes the service was registered under and the
   *         Java Runtime Environment supports permissions.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @throws IllegalArgumentException If the specified
   * {@code ServiceReference} was not created by the same framework
   *         instance as this {@code BundleContext}.
   * @see #ungetService(ServiceReference)
   * @see ServiceFactory
   */

  def getService[S](reference: ServiceReference[S]): S = {
    reference match {
      case o: OSGiServiceReferenceImpl[S] => o.getService()
      case t => throw new IllegalArgumentException("Invalid service reference: " + t.getClass)
    }
  }

  /**
   * Releases the service object referenced by the specified
   * {@code ServiceReference} object. If the context bundle's use count for
   * the service is zero, this method returns {@code false}. Otherwise, the
   * context bundle's use count for the service is decremented by one.
   *
   * <p>
   * The service's service object should no longer be used and all references
   * to it should be destroyed when a bundle's use count for the service drops
   * to zero.
   *
   * <p>
   * The following steps are required to unget the service object:
   * <ol>
   * <li>If the context bundle's use count for the service is zero or the
   * service has been unregistered, {@code false} is returned.
   * <li>The context bundle's use count for this service is decremented by
   * one.
   * <li>If the context bundle's use count for the service is currently zero
   * and the service was registered with a {@code ServiceFactory} object, the
   * {@link ServiceFactory#ungetService(Bundle, ServiceRegistration, Object)}
   * method is called to release the service object for the context bundle.
   * <li>{@code true} is returned.
   * </ol>
   *
   * @param reference A reference to the service to be released.
   * @return {@code false} if the context bundle's use count for the service
   *         is zero or if the service has been unregistered; {@code true}
   *         otherwise.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @throws IllegalArgumentException If the specified
   * {@code ServiceReference} was not created by the same framework
   *         instance as this {@code BundleContext}.
   * @see #getService
   * @see ServiceFactory
   */
  def ungetService(reference: ServiceReference[_]): Boolean = {
    reference match {
      case o: OSGiServiceReferenceImpl[_] => o.ungetService
      case t => throw new IllegalArgumentException("Invalid service reference: " + t.getClass)
    }
  }

  /**
   * Creates a {@code File} object for a file in the persistent storage area
   * provided for the bundle by the Framework. This method will return
   * {@code null} if the platform does not have file system support.
   *
   * <p>
   * A {@code File} object for the base directory of the persistent storage
   * area provided for the context bundle by the Framework can be obtained by
   * calling this method with an empty string as {@code filename}.
   *
   * <p>
   * If the Java Runtime Environment supports permissions, the Framework will
   * ensure that the bundle has the {@code java.io.FilePermission} with
   * actions {@code read},{@code write},{@code delete} for all files
   * (recursively) in the persistent storage area provided for the context
   * bundle.
   *
   * @param fileName A relative name to the file to be accessed.
   * @return A {@code File} object that represents the requested file or
   * {@code null} if the platform does not have file system support.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   */
  def getDataFile(fileName: String): File = {
    bundle.getDataFile(fileName)
  }

  /**
   * Creates a {@code Filter} object. This {@code Filter} object may be used
   * to match a {@code ServiceReference} object or a {@code Dictionary}
   * object.
   *
   * <p>
   * If the filter cannot be parsed, an {@link InvalidSyntaxException} will be
   * thrown with a human readable message where the filter became unparsable.
   *
   * @param filter The filter string.
   * @return A {@code Filter} object encapsulating the filter string.
   * @throws InvalidSyntaxException If {@code filter} contains an invalid
   *         filter string that cannot be parsed.
   * @throws NullPointerException If {@code filter} is null.
   * @throws IllegalStateException If this BundleContext is no longer valid.
   * @see "Framework specification for a description of the filter string syntax."
   * @see FrameworkUtil#createFilter(String)
   * @since 1.1
   */
  def createFilter(filter: String): Filter = {
    try {
      new FilterImpl(mod.LDAPExpr(filter))
    }
    catch {
      case e: IllegalArgumentException => throw new InvalidSyntaxException(e.getMessage, filter, e)
    }
  }
}
