/*******************************************************************************
 * Copyright (c) 2007 - 2015 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.filesystems.impl;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ResourcesPlugin;

public class JarAccessFactory {
	
	private static Map<String, JarAccess> jars = new HashMap<String, JarAccess>();
	
	static {
		ResourcesPlugin.getWorkspace().addResourceChangeListener(new WorkspaceListener());
	}

	public synchronized static JarAccess getJarAccess(String location, JarSystemImpl context) {
		JarAccess jar = jars.get(location);
		if(jar == null) {
			jar = new JarAccess();
			jar.setMain(context);
			jar.setLocation(location);
			jars.put(location, jar);
		}
		if(context != jar.getMain()) {
			jar.addSlave(context);
		}
		return jar;
	}

	private synchronized static void onProjectDelete(IProject project) {
		for (JarAccess jar: jars.values()) {
			jar.onProjectDelete(project);
		}
	}
	
	static class WorkspaceListener implements IResourceChangeListener {

		@Override
		public void resourceChanged(IResourceChangeEvent event) {
			if(event.getType() == IResourceChangeEvent.PRE_DELETE
					&& event.getResource() instanceof IProject) {
				onProjectDelete((IProject)event.getResource());
			}
		}		
	}

}
