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

package org.chronologicalthought.modula.scratch

import org.chronologicalthought.modula.Module
import org.chronologicalthought.modula.ModuleContext._
import java.lang.IllegalStateException

/**
 * User: dave
 */

class ServiceIdeas {

  def serviceUsage {
    class Foo {
      def bar = "bar"
    }

    val reg = this.whenRegistered(classOf[Foo]) {
      foo => println("New " + foo.bar)
    }

    this.registerMy(new Foo)

    println("Found " + this.withAny(classOf[Foo])(_.bar).getOrElse(throw new IllegalStateException("Missing foo service")))

    reg.unregister()

    this.register(new Foo)
  }

  def moduleUsage {
    this.whenRegistered(classOf[Module]) {
      module => {
        module.resources(_ == "OSGI-INF/persistence.xml").foreach {
          url => {
            val node = xml.XML.load(url)
            node match {
              case <foo>
                {content}
                </foo> => println("Found " + content)
            }
          }
        }
      }
    }
  }
}
