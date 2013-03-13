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

package org.chronologicalthought.modula.osgi.deprecated

import org.osgi.service.packageadmin.PackageAdmin
import org.osgi.framework.Bundle

/**
 * User: dave
 */

class PackageAdminImpl extends PackageAdmin {
  def getExportedPackages(bundle: Bundle) = null

  def getExportedPackages(name: String) = null

  def getExportedPackage(name: String) = null

  def refreshPackages(bundles: Array[Bundle]) {}

  def resolveBundles(bundles: Array[Bundle]) = false

  def getRequiredBundles(symbolicName: String) = null

  def getBundles(symbolicName: String, versionRange: String) = null

  def getFragments(bundle: Bundle) = null

  def getHosts(bundle: Bundle) = null

  def getBundle(clazz: Class[_]) = null

  def getBundleType(bundle: Bundle) = 0
}