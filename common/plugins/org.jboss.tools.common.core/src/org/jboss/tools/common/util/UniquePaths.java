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

	public IPath intern(IPath path) {
		IPath result = paths.get(path);
		if(result == null) {
			paths.put(path, path);
			result = path;
			if(paths.size() % 100 == 0) System.out.println("--->" + paths.size());
		}
		return result;
	}

}
