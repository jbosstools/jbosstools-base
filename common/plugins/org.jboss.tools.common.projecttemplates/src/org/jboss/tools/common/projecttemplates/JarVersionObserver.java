/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.projecttemplates;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class JarVersionObserver {
	static String JAR_SUFFIX = ".jar"; //$NON-NLS-1$
	File folder;
	Set<File> existedJars = new HashSet<File>();
	Map<File, JarVersionObserver> children = new HashMap<File, JarVersionObserver>();

	public JarVersionObserver(File f) {
		folder = f;
		init();
	}

	public boolean containsJars() {
		return !existedJars.isEmpty() || !children.isEmpty();
	}

	private void init() {
		if(!folder.isDirectory()) return;
		File[] fs = folder.listFiles();
		if(fs == null) return;
		for (File f: fs) {
			if(f.isDirectory()) {
				JarVersionObserver c = new JarVersionObserver(f);
				if(!c.containsJars()) continue;
				children.put(f, c);
			} else if(f.isFile()) {
				String n = f.getName();
				if(n.toLowerCase().endsWith(JAR_SUFFIX)) {
					existedJars.add(f);
				}
			}
		}
	}

	public void execute() {
		File[] fs = folder.listFiles();
		if(fs == null) return;
		Set<File> oldJars = new HashSet<File>();
		Set<File> newJars = new HashSet<File>();
		for (File f: fs) {
			if(f.isDirectory()) {
				JarVersionObserver c = children.get(f);
				if(c != null) c.execute();
			} else if(f.isFile()) {
				String n = f.getName();
				if(n.toLowerCase().endsWith(JAR_SUFFIX)) {
					if(!existedJars.contains(f)) {
						newJars.add(f);
					} else {
						oldJars.add(f);
					}
				}
			}
		}
		filter(oldJars, newJars);
	}

	public void filter(Set<File> oldJars, Set<File> newJars) {
		if(oldJars.isEmpty() || newJars.isEmpty()) return;
		for (File f: newJars) {
			String n1 = f.getName().substring(0, f.getName().length() - JAR_SUFFIX.length());
			Iterator<File> it = oldJars.iterator();
			while(it.hasNext()) {
				File o = it.next();
				String n2 = o.getName().substring(0, o.getName().length() - JAR_SUFFIX.length());
				if(areEqualNamesOfDifferentVersions(n1, n2)) {
					it.remove();
					o.delete();
				}
			}
		}
	}

	static boolean areEqualNamesOfDifferentVersions(String n1, String n2) {
		int d1 = n1.lastIndexOf('-');
		int d2 = n2.lastIndexOf('-');
		String base1 = d1 < 0 ? n1 : n1.substring(0, d1);
		String base2 = d2 < 0 ? n2 : n2.substring(0, d2);
		if(!base1.equals(base2)) return false;
		String regex = "([0-9]+\\.)+([^\\.]+)"; //$NON-NLS-1$
		boolean isVersion1 = d1 < 0 || n1.substring(d1 + 1).matches(regex);
		boolean isVersion2 = d2 < 0 || n2.substring(d2 + 1).matches(regex);
		
		return isVersion1 && isVersion2;
	}

}
