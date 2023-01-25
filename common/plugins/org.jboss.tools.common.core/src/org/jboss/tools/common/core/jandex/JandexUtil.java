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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.jboss.jandex.AnnotationInstance;
import org.jboss.jandex.ClassInfo;
import org.jboss.jandex.DotName;
import org.jboss.jandex.Index;
import org.jboss.jandex.Indexer;
import org.jboss.tools.common.core.CommonCorePlugin;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class JandexUtil {

	/**
	 * Parses content of a jar file into org.jboss.jandex.Index that allows to check
	 * quickly if the jar has classes inheriting certain types or using certain
	 * annotations.
	 * 
	 * @param jarFile
	 * @param indexer
	 * @return
	 * @throws IOException
	 */
	static Index createJarIndex(File jarFile, Indexer indexer) throws IOException {
		try (JarFile jar = new JarFile(jarFile)) {
			Enumeration<JarEntry> entries = jar.entries();
			while (entries.hasMoreElements()) {
				JarEntry entry = entries.nextElement();
				if (entry.getName().endsWith(".class")) {
					try (InputStream stream = jar.getInputStream(entry)) {
						indexer.index(stream);
					} catch (IOException e) {
						CommonCorePlugin.getPluginLog().logError(e);
					}
				}
			}
			return extracted(indexer);
		}
	}

	private static Index extracted(Indexer indexer) {
		return indexer.complete();
	}

	/**
	 * Implement to use in hasAnnotatedType(File, IAnnotationCheck)
	 */
	public static interface IAnnotationCheck {
		/**
		 * Called by hasAnnotatedType(File, IAnnotationCheck)
		 * 
		 * @param annotationType
		 * @return
		 */
		public boolean isRelevant(String annotationType);
	}

	/**
	 * Returns true if for at least one annotation on at least one type in jar file
	 * method IAnnotationCheck.isRelevant() returns true. Returns false if all
	 * checks fail.
	 * 
	 * @param jarFile
	 * @param check
	 * @return
	 */
	public static boolean hasAnnotation(File jarFile, IAnnotationCheck check) {
		try {
			Indexer indexer = new Indexer();
			Index index = JandexUtil.createJarIndex(jarFile, indexer);

			for (ClassInfo cls : index.getKnownClasses()) {
				for (Map.Entry<DotName, List<AnnotationInstance>> es : cls.annotationsMap().entrySet()) {
					String typeName = es.getKey().toString();
					if (check.isRelevant(typeName)) {
						return true;
					}
				}
			}
		} catch (IOException e) {
			CommonCorePlugin.getPluginLog().logError(e);
		}
		return false;
	}

	public static boolean hasSubtypes(File jarFile, List<String> classes, List<String> interfaces) {
		try {
			Indexer indexer = new Indexer();
			Index index = JandexUtil.createJarIndex(jarFile, indexer);

			for (String className : classes) {
				if (!index.getAllKnownSubclasses(DotName.createSimple(className)).isEmpty()) {
					return true;
				}
			}
			for (String className : interfaces) {
				if (!index.getAllKnownImplementors(DotName.createSimple(className)).isEmpty()) {
					return true;
				}
			}
		} catch (IOException e) {
			CommonCorePlugin.getPluginLog().logError(e);
		}
		return false;
	}

}
