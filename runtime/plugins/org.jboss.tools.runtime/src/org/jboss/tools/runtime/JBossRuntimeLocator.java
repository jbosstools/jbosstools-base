/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.runtime;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.wst.server.core.model.RuntimeLocatorDelegate;
import org.jboss.tools.runtime.bean.ServerBean;
import org.jboss.tools.runtime.bean.ServerBeanLoader;
import org.jboss.tools.runtime.bean.ServerType;

public class JBossRuntimeLocator extends RuntimeLocatorDelegate {

	public JBossRuntimeLocator() {
	}

	@Override
	public void searchForRuntimes(IPath path, IRuntimeSearchListener listener,
			IProgressMonitor monitor) {
		ServerBeanLoader loader = new ServerBeanLoader();
		List<ServerDefinition> serverDefinitions = new ArrayList<ServerDefinition>();
		searchForRuntimes(serverDefinitions, path, loader, 4, monitor);
		JBossRuntimeStartup runtimeStartup = new JBossRuntimeStartup();
		runtimeStartup.initializeRuntimes(serverDefinitions);
	}
	
	public void searchForRuntimes(String path, IProgressMonitor monitor) {
		searchForRuntimes(new Path(path), null, monitor);
	}
	
	private void searchForRuntimes(List<ServerDefinition> serverDefinitions, IPath path, ServerBeanLoader loader, int depth, IProgressMonitor monitor) {
		File[] children = null;
		if (path != null) {
			File root = path.toFile();
			ServerBean serverBean = loader.loadFromLocation(root);
			
			if (!ServerType.UNKNOWN.equals(serverBean.getType())) {
				serverDefinitions.add(new ServerDefinition(serverBean));
			} else {
				children = root.listFiles();
			}
		} else {
			children = File.listRoots();
		}
		if (depth == 0) {
			return; 
		}
		if( children != null ) {
			monitor.beginTask("Searching for JBoss runtime...", children.length); //$NON-NLS-1$
			for( int i = 0; i < children.length; i++ ) {
				if( children[i].isDirectory()) {
					searchForRuntimes(serverDefinitions, new Path(children[i].getAbsolutePath()), loader, --depth, monitor);
				}
			}
		}
		monitor.done();
	}	

}
