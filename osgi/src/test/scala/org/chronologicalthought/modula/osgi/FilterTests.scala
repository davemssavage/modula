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

import org.junit.Test
import org.junit.Assert._
import java.util.{Collections, HashMap, Dictionary, Hashtable}
import org.osgi.framework.InvalidSyntaxException

/**
 * User: dave
 */

class FilterTests {

  class Foo

  @Test
  def filterReference {
    val f = newFramework()
    val ctx = f.getBundleContext
    val props = new Hashtable[String, String]()
    props.put("foo", "bar")
    val reg = ctx.registerService(classOf[Foo].getName(), new Foo(), props)
    val ref = reg.getReference

    val filter = ctx.createFilter("(foo=bar)")
    assertTrue(filter.`match`(ref))
  }

  @Test
  def filterNullDictionary {
    val f = newFramework()
    val ctx = f.getBundleContext

    val props: Dictionary[String, String] = null
    assertFalse(ctx.createFilter("(foo=bar)").matchCase(props))
    assertTrue(ctx.createFilter("(!(foo=bar))").matchCase(props))
  }

  @Test
  def filterDictionary {
    val f = newFramework()
    val ctx = f.getBundleContext

    val props: Dictionary[String, String] = new Hashtable[String, String]()
    props.put("foo", "bar")

    val filter = ctx.createFilter("(foo=bar)")
    assertTrue(filter.matchCase(props))
  }

  @Test
  def filterDictionaryInsensitive {
    val f = newFramework()
    val ctx = f.getBundleContext

    val props: Dictionary[String, String] = new Hashtable[String, String]()
    props.put("foo", "bar")

    val filter = ctx.createFilter("(Foo=bar)")
    assertTrue(filter.`match`(props))
  }

  @Test
  def filterNullDictionaryInsensitive {
    val f = newFramework()
    val ctx = f.getBundleContext

    val props: Dictionary[String, String] = null
    assertFalse(ctx.createFilter("(foo=bar)").`match`(props))
    assertTrue(ctx.createFilter("(!(foo=bar))").`match`(props))
  }

  @Test
  def filterMap {
    val f = newFramework()
    val ctx = f.getBundleContext

    val props = new HashMap[String, String]()
    props.put("foo", "bar")

    val filter = ctx.createFilter("(foo=bar)")
    assertTrue(filter.matches(props))
  }

  @Test
  def filterNullMap {
    val f = newFramework()
    val ctx = f.getBundleContext

    val props: HashMap[String, String] = null

    assertFalse(ctx.createFilter("(foo=bar)").matches(props))
    assertTrue(ctx.createFilter("(!(foo=bar))").matches(props))
  }

  @Test
  def invalidFilters {
    val f = newFramework()
    val ctx = f.getBundleContext

    def assertFilterInvalid(str: String) = {
      try {
        ctx.createFilter(str)
        fail("expected InvalidSyntaxException exception")
      }
      catch {
        case e: InvalidSyntaxException => // expected
      }
    }

    assertFilterInvalid("")
  }

  private def newFramework() = {
    val framework = new FrameworkFactoryImpl().newFramework(Collections.emptyMap())
    framework.start()
    framework
  }

}
