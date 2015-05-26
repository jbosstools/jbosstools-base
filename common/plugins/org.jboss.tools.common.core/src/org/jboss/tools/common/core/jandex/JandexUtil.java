/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.core.jandex;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.jboss.jandex.DotName;
import org.jboss.jandex.Index;
import org.jboss.jandex.Indexer;
import org.jboss.tools.common.core.CommonCorePlugin;

public class JandexUtil {

	/**
	 * Parses content of a jar file into org.jboss.jandex.Index that allows to check quickly
	 * if the jar has classes inheriting certain types or using certain annotations.  
	 * 
	 * @param jarFile
	 * @param indexer
	 * @return
	 * @throws IOException
	 */
	public static Index createJarIndex(File jarFile, Indexer indexer) throws IOException {
		JarFile jar = new JarFile(jarFile);
		try {
			Enumeration<JarEntry> entries = jar.entries();
			while (entries.hasMoreElements()) {
				JarEntry entry = entries.nextElement();
				if (entry.getName().endsWith(".class")) {
					try {
						InputStream stream = jar.getInputStream(entry);
						try {
							indexer.index(stream);
						} finally {
							stream.close();
						}
                    } catch (IOException e) {
                    	CommonCorePlugin.getPluginLog().logError(e);
                    }
                }
            }
            return indexer.complete();
        } finally {
            safeClose(jar);
        }
    }

    private static void safeClose(Closeable close) {
        try {
            close.close();
        } catch (IOException ignore) {
        	//ignore
        }
    }

}
