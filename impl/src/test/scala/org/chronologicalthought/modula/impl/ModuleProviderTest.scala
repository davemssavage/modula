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

import org.junit.Assert._
import org.mockito.Mockito._
import org.mockito.Matchers._

import org.junit.Test
import org.chronologicalthought.modula.LDAPExpr._
import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock
import org.chronologicalthought.modula._
import org.chronologicalthought.modula.{Empty, Box, Full}

/**
 * @author David Savage
 */

class ModuleProviderTest {
  @Test
  def registerUnregisterModuleProvider() {
    val context = newContext()

    val provider = mock(classOf[ModuleProvider])

    val registration = context.register(provider, classOf[ModuleProvider])

    assertAndGetModuleFrom(context, 1)

    registration.unregister()

    assertEquals(1, context.modules.size)
  }

  @Test
  def findModuleByProvider() {
    def findModule() {
      val context = newContext()
      val provider = mock(classOf[ModuleProvider])

      context.register(provider, classOf[ModuleProvider])

      var found = context.findServices(classOf[Module])
      found = found.filter(_.isProvidedBy(provider))

      assertEquals(1, found.size)
    }

    for (i <- 1 to 100) {
      findModule
    }
  }

  @Test
  def initDestroyModuleProvider() {
    val context = newContext()

    val provider = mock(classOf[ModuleProvider])

    val registration = context.register(provider, classOf[ModuleProvider])

    verify(provider).init(isA(classOf[ResourceLoader]))

    registration.unregister()

    verify(provider).destroy()
  }

  @Test
  def moduleProviderCallback() {
    val context = newContext()

    val provider = mock(classOf[ModuleProvider])

    val registration = context.register(provider, classOf[ModuleProvider])

    val module = assertAndGetModuleFrom(context, 1)

    module.start()

    verify(provider).start()

    module.stop()

    verify(provider).stop()

    module.loadClass("foo")

    verify(provider).loadClass("foo")

    registration.unregister()

    verify(provider).destroy()
  }

  val ep = Constants.PackageNamespace

  def imports(packageName: String, provider: ModuleProvider) = {
    import Constants.Directives.RequirementFilter
    new Requirement(ep, Map.empty, Map(RequirementFilter -> expr(ep, `=`, packageName)), Some(provider))
  }

  def exports(packageName: String, provider: ModuleProvider) = {
    new Capability(ep, Map(ep -> packageName), Map.empty, provider)
  }

  @Test
  def moduleProviderClassLoading() {
    val config = Map(FrameworkImplConstants.ParentClassLoader -> new ClassLoader() {
      override def loadClass(name: String, resolve: Boolean) = {
        throw new ClassNotFoundException(name)
      }
    })
    val context = newContext(config)

    val fooProvider = mock(classOf[ModuleProvider])
    val barProvider = mock(classOf[ModuleProvider])

    class Foo
    class Bar

    when(fooProvider.capabilities).thenReturn(Nil)
    when(fooProvider.requirements).thenAnswer(new Answer[Traversable[Requirement]]() {
      def answer(p1: InvocationOnMock) = {
        imports(classOf[Bar].getPackage.getName, fooProvider) :: Nil
      }
    })

    when(barProvider.capabilities).thenAnswer(new Answer[Traversable[Capability]]() {
      def answer(p1: InvocationOnMock) = {
        exports(classOf[Bar].getPackage.getName, barProvider) :: Nil
      }
    })
    when(barProvider.requirements).thenReturn(Nil)

    var cl: ClassLoader = null

    when(fooProvider.init(isA(classOf[ResourceLoader]))).thenAnswer(new Answer[Unit] {
      def answer(p1: InvocationOnMock) = {
        cl = p1.getArguments()(0).asInstanceOf[ClassLoader]
      }
    })

    when(fooProvider.loadClass(isA(classOf[String]))).thenAnswer(new Answer[Box[Class[_]]]() {
      def answer(p1: InvocationOnMock) = {
        val cn = p1.getArguments()(0).asInstanceOf[String]
        if (cn == classOf[Foo].getName)
          Full(classOf[Foo])
        else {
          try {
            Full(cl.loadClass(cn))
          }
          catch {
            case c: ClassNotFoundException => None
          }
        }
      }
    })

    when(barProvider.loadClass(isA(classOf[String]))).thenAnswer(new Answer[Box[Class[_]]]() {
      def answer(p1: InvocationOnMock) = {
        val cn = p1.getArguments()(0).asInstanceOf[String]
        if (cn == classOf[Bar].getName)
          Full(classOf[Bar])
        else {
          Empty
        }
      }
    })


    context.register(fooProvider, classOf[ModuleProvider])
    context.register(barProvider, classOf[ModuleProvider])

    val module = assertAndGetModuleFrom(context, 1)

    val foo = module.loadClass(classOf[Foo].getName)

    verify(fooProvider).loadClass(classOf[Foo].getName)

    assertSame(classOf[Foo], foo.get)

    val bar = module.loadClass(classOf[Bar].getName)

    verify(fooProvider).loadClass(classOf[Bar].getName)
    verify(barProvider).loadClass(classOf[Bar].getName)

    assertSame(classOf[Bar], bar.get)
  }


  @Test
  def moduleProviderStateHandling() {
    val context = newContext()

    val provider = mock(classOf[ModuleProvider])

    val registration = context.register(provider, classOf[ModuleProvider])

    val module = assertAndGetModuleFrom(context, 1)

    module.start()

    module.start()

    verify(provider, times(1)).start()

    module.stop()

    module.stop()

    verify(provider, times(1)).stop()
  }

  private def newContext(config: Map[String, Any] = Map.empty) = {
    val framework = FrameworkFactoryImpl.newFramework(config)
    framework.start()
    framework.context
  }

  private def assertAndGetModuleFrom(context: ModuleContext, id: Long) = {
    context.findModule(id).openOr(throw new AssertionError("Missing module"))
  }

}

