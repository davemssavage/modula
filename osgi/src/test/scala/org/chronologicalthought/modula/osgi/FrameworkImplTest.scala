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

import org.junit.Assert._
import org.junit.Test
import java.io.File
import org.osgi.framework.{Constants, BundleException}
import collection.JavaConversions._

/**
 * @author David Savage
 */

class FrameworkImplTest {
  private val root = "/tmp/modula"
  private val config = Map[String, String](Constants.FRAMEWORK_STORAGE -> root)

  @Test
  def initStartStop {
    val config = Map[String, String]()
    val framework = new FrameworkFactoryImpl().newFramework(config)
    framework.init
    framework.start()
    framework.stop()
  }

  @Test
  def installUninstallBundle = {
    val name = "org.junit.Assert"

    val framework = newFramework()

    val b = framework.getBundleContext.installBundle("file:%s/.ivy2/cache/org.junit/com.springsource.org.junit/jars/com.springsource.org.junit-4.11.0.jar".format(System.getProperty("user.home")))

    assertNotNull(b.loadClass(name))

//    val file = new File(root, b.getBundleId + "/cache/lib/test/com.springsource.org.junit_4.5.0.jar")
//
//    assertTrue("Failed to create " + file, file.exists)
//
    b.uninstall

    try {
      b.loadClass(name)
      fail("Should not be able to load class from uninstalled bundle")
    }
    catch {
      case e: IllegalStateException => // expected
    }

    //assertFalse("Failed to delete " + file, file.exists)

    framework.stop()
  }

  @Test(expected = classOf[BundleException])
  def installUnknownBundle {
    val framework = newFramework()
    framework.getBundleContext.installBundle("file:wibble.jar")
  }

  @Test
  def systemExtraPackages() {
    val extra = """org.osgi.framework.startlevel;version="1.0",org.osgi.framework.wiring;version="1.0",org.osgi.framework;version="1.6",org.osgi.framework.hooks.service;version="1.1",org.osgi.framework.hooks.resolver;version="1.0",org.osgi.framework.launch;version="1.0",org.osgi.framework.hooks.bundle;version="1.0",org.osgi.framework.hooks.weaving;version="1.0",org.osgi.service.packageadmin;version="1.2",org.chronologicalthought.modula;version="1.0",scala.annotation.unchecked;version="1.0",scala.reflect.generic;version="1.0",scala.collection.interfaces;version="1.0",scala.xml.persistent;version="1.0",scala.concurrent;version="1.0",scala.actors.threadpool;version="1.0",scala.util.parsing.combinator.testing;version="1.0",scala.annotation;version="1.0",scala.concurrent.forkjoin;version="1.0",scala.reflect;version="1.0",scala.io;version="1.0",scala.util.automata;version="1.0",scala.ref;version="1.0",scala.util.parsing.syntax;version="1.0",scala.util.matching;version="1.0",scala.util.parsing.combinator;version="1.0",scala.compat;version="1.0",scala.runtime;version="1.0",scala.util.parsing.ast;version="1.0",scala.util.logging;version="1.0",scala.util.parsing.json;version="1.0",scala.xml.dtd;version="1.0",scala.util.parsing.combinator.token;version="1.0",scala.util.parsing.input;version="1.0",scala.testing;version="1.0",scala.util.grammar;version="1.0",scala.collection.immutable;version="1.0",scala.collection;version="1.0",scala.text;version="1.0",scala.actors.scheduler;version="1.0",scala;version="1.0",scala.actors;version="1.0",scala.util.continuations;version="1.0",scala.xml.include;version="1.0",scala.math;version="1.0",scala.collection.script;version="1.0",scala.util;version="1.0",scala.util.control;version="1.0",scala.actors.threadpool.helpers;version="1.0",scala.xml.parsing;version="1.0",scala.annotation.target;version="1.0",scala.xml;version="1.0",scala.actors.remote;version="1.0",scala.collection.mutable;version="1.0",scala.actors.threadpool.locks;version="1.0",scala.util.regexp;version="1.0",scala.xml.include.sax;version="1.0",scala.xml.factory;version="1.0",scala.xml.transform;version="1.0",scala.util.parsing.combinator.lexical;version="1.0",scala.util.parsing.combinator.syntactical;version="1.0",scala.xml.pull;version="1.0",scala.mobile;version="1.0",scala.collection.generic;version="1.0",junit.awtui;version="3.8.2",junit.extensions;version="3.8.2",junit.framework;version="3.8.2",junit.runner;version="3.8.2",junit.swingui;version="3.8.2",junit.swingui.icons;version="3.8.2",junit.textui;version="3.8.2""""
    val cfExtra = Map(Constants.FRAMEWORK_SYSTEMPACKAGES_EXTRA -> extra) ++ config
    val framework = newFramework(cfExtra)
    // TODO assert extra packages have some use
  }


  private def newFramework(cf: Map[String, String] = config) = {
    val framework = new FrameworkFactoryImpl().newFramework(cf)
    framework.start()
    framework
  }
}
