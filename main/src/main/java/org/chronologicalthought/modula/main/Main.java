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

package org.chronologicalthought.modula.main;

import org.chronologicalthought.modula.osgi.FrameworkFactoryImpl;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleException;
import org.osgi.framework.launch.Framework;
import org.osgi.framework.launch.FrameworkFactory;

import java.io.File;
import java.io.FilenameFilter;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.ServiceLoader;

/**
 * @author David Savage
 */

public class Main {
    public static void main(String[] args) throws BundleException, InterruptedException, MalformedURLException {
        FrameworkFactory factory = loadFactory();
        Framework fw = factory.newFramework(Collections.<String, String>emptyMap());
        fw.start();

        File dir = new File(args[0]);

        ArrayList<Bundle> bundles = new ArrayList<Bundle>();

        FilenameFilter filter = new FilenameFilter() {
            public boolean accept(File file, String s) {
                return s.endsWith(".jar");
            }
        };

        for (File f : dir.listFiles(filter)) {
            Bundle b;
            b = fw.getBundleContext().installBundle(f.toURI().toURL().toExternalForm());
            bundles.add(b);
        }


        for (Bundle bb : bundles) {
            try {
                bb.start();
            } catch (BundleException e) {
                e.printStackTrace();
            }
        }

        fw.waitForStop(0);
    }

    private static FrameworkFactory loadFactory() {
        return new FrameworkFactoryImpl();
    }
//    private static FrameworkFactory loadFactory() {
//        ServiceLoader<FrameworkFactory> loader = ServiceLoader.load(FrameworkFactory.class, Main.class.getClassLoader());
//        if (!loader.iterator().hasNext()) {
//            throw new IllegalStateException("Missing framework factory");
//        }
//
//        return loader.iterator().next();
//    }
}
