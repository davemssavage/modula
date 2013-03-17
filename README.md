Modula
======

Modula is a thought experiment in modularity written using the Scala language.

At the present time it is <em>unfinished</em> and <em>highly likely</em> to change so if you don't want to be hacking
around with <b>breaking changes</b> this is <em>not the project you're looking for</em>.

Build Status
--------

[![Build Status](https://travis-ci.org/davemssavage/modula.png)](https://travis-ci.org/davemssavage/modula)

Background
--------
I started to write Modula as an evening project to help me improve my knowledge of the Scala
programming language. I've been using Scala as a hobby language on and off for a couple of years but have so far really
only tinkered around the edges. However I find the best way to really learn something new is to set a hard problem and
then figure out how to solve it. So after reading the rather excellent
<a href="http://www.artima.com/shop/programming_in_scala_2ed"> Programming In Scala</a> last year, I decided to do
something more interesting than the standard Hello World as an Xmas project.

Having worked extensively with OSGi in the past - I've worked on the specifications group and helped write
<a href="http://www.manning.com/hall/">a book</a> on the subject - I wondered if the Scala language could improve on the
existing OSGi API's. Given how well thought out the OSGi specification is, this sounded like a <em>hard problem</em> so
I immediately started experimenting :)

After a couple of months of tinkering I have something that can mimic a several of the core features of OSGi including:

* service discovery
* dependency resolution
* uses constraints
* fragment attachement

However there are probably more missing features than completed ones, most notably uninstall is only implemented to a
very basic level and does not currently re-resolve dependencies after a change.

I've decided to open source this project on Github as a way to see if there was any interest in this other than myself
and hopefully solicit some help in implementing more features.

Features
--------

As mentioned above, Modula is highly influenced by the OSGi specification but adds several unique features that make it
interesting in it's own right.

The basic building blocks in Modula are:

 * Services - services are simply objects, they can be shared via a ModuleContext and act as building blocks to build
 more complex services
 * ModuleContext - a module context provides a mechanism to share services within a VM, it has methods to make it easy
 to register new services or discover existing ones
 * Module - a service that provides class and resource loading capabilities, modules may provide or require capabilities
 they are wired together via a Resolver
 * Framework - the framework is at the root of all Modula applications, it is a Module and provides a ModuleContext
 * ModuleProvider - a service that can be registered to install a new module within the framework.

It is worth restating one of the points from above, <b>Modules</b> <em>are</em> <b>Services</b>! This was pretty tricky to
implement but the fact that it seems to be possible to implement a Modularity layer on top of a Services layer seems
kinda cool :)

Project Structure
-------------

The source code for this project is broken down into several sub projects

 * api - Defines the Modula API
 * factory - Provides a way to instantiate an instance of a Modula framework
 * impl - Where the majority of the work is done
 * osgi - An experiment to see if it is possible to implement an OSGi framework on top of a Modula framework
 * main - Trivial project to start the Modula OSGi framework
 * tools - Barious utilities I cooked up to help in testing.

Building
--------------

The project is built using SBT so to build and test the framework you can do:

    $ sbt clean test

Launching
--------------
I've set up an assembly target on the main sub project to allow you to simply boot modula and point at some OSGi
bundles as a test case.

    $ sbt clean assembly
    $ java -jar main/target/modula-main-assembly-1.0.jar <path-to-bundles>

Gogo
---------------

As a simple test case I've been booting the Gogo shell environment in Modula:

    $ java -jar main/target/modula-main-assembly-1.0.jar gogo
    g! ls gogo
    /Users/dave/development/chronological-thought/modula/gogo/org.apache.felix.gogo.command-0.12.0.jar
    /Users/dave/development/chronological-thought/modula/gogo/org.apache.felix.gogo.runtime-0.10.0.jar
    /Users/dave/development/chronological-thought/modula/gogo/org.apache.felix.gogo.shell-0.10.0.jar
    g! lb
    Start Level service is unavailable.
       ID|State      |Name
        0|Active     |system.bundle (0.0.0)
        1|Active     |Apache Felix Gogo Command (0.12.0)
        2|Active     |Apache Felix Gogo Runtime (0.10.0)
        3|Active     |Apache Felix Gogo Shell (0.10.0)
