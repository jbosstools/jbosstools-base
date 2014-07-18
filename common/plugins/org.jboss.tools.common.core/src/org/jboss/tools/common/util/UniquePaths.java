/*******************************************************************************
  * Copyright (c) 2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.util;

import java.util.Hashtable;
import java.util.Iterator;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;

/**
 * 
 * Even a huge workspace contains much less than 100000 paths, whereas even in a middle
 * workspace with about 10000 paths as much as 250000 instances of Path are kept in various 
 * caches. 
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class UniquePaths {
	static UniquePaths instance = new UniquePaths();
	
	public static UniquePaths getInstance() {
		return instance;
	}

	Hashtable<IPath, IPath> paths = new Hashtable<IPath, IPath>();

	private UniquePaths() {}

	/**
	 * IResource.getFullPath() and IResource.getLocation() return new object
	 * at each invocation. All clients that need storing resource paths,
	 * may obtain the single instance stored by this class. 
	 * This significantly decreases the number of stored objects.
	 * 
	 * @param path object returned by IResource.getFullPath() or IResource.getLocation()
	 * @return unique object equal to the given path.
	 */
	public synchronized IPath intern(IPath path) {
		IPath result = paths.get(path);
		if(result == null) {
			paths.put(path, path);
			result = path;
		}
		return result;
	}

	/**
	 * When a resource is removed from workspace, the cash of stored 
	 * paths should be cleaned from obsolete elements.
	 * 
	 * @param removed
	 */
	public synchronized void clean(IResource removed) {
		IPath fullPath = removed.getFullPath();
		IPath location = removed.getLocation();
		Iterator<IPath> it = paths.keySet().iterator();
		while(it.hasNext()) {
			IPath p = it.next();
			if(fullPath.isPrefixOf(p)) {
				it.remove();
			} else if(location != null && location.isPrefixOf(p)) {
				it.remove();
			}
		}
	}

}
