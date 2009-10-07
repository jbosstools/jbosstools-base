/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.projecttemplates;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jboss.tools.common.util.FileUtil;
import org.jboss.tools.common.zip.UnzipOperation;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

public class ProjectTemplatesPlugin extends AbstractUIPlugin{
	public static final String PLUGIN_ID = "org.jboss.tools.common.projecttemplates"; //$NON-NLS-1$
	static ProjectTemplatesPlugin instance;
	
	public static ProjectTemplatesPlugin getDefault() {
		if(instance == null) {
			Platform.getBundle(PLUGIN_ID);
		}
		return instance;
	}
	
	public ProjectTemplatesPlugin() {
	    super();
	    instance = this;
	}
	
	public void start(BundleContext context) throws Exception {
		super.start(context);
		copyProjectTemplates();
	}


	void copyProjectTemplates() throws IOException {
		Bundle bundle = Platform.getBundle(PLUGIN_ID);
		File location = Platform.getStateLocation(bundle).toFile();
		File install = FileLocator.getBundleFile(bundle);  
		if(install.isDirectory()) {
			FileFilter filter = new FileFilter() {
				public boolean accept(File pathname) {
					return pathname != null 
					&& !"CVS".equals(pathname.getName()) //$NON-NLS-1$
					&& !".svn".equalsIgnoreCase(pathname.getName()); //$NON-NLS-1$
				}
			};
			copy(location, install, "templates", filter); //$NON-NLS-1$
			copy(location, install, "lib", filter); //$NON-NLS-1$
		} else {
			UnzipOperation unzip = new UnzipOperation(install);
			unzip.execute(location.getAbsolutePath(), "templates");
			unzip.execute(location.getAbsolutePath(), "lib");
		}
	}
	
	private void copy(File location, File install, String name, FileFilter filter) {
		location = new File(location, name);
		//if(location.isDirectory()) return;
		install = new File(install, name);
		location.mkdirs();
		FileUtil.copyDir(install, location, true, true, true, filter);
	}
	
	public static String getTemplateStateLocation() {
		String stateLocation = getTemplateStatePath().toString().replace('\\', '/');
		if(!stateLocation.endsWith("/")) stateLocation += "/";  //$NON-NLS-1$ //$NON-NLS-2$
		return stateLocation;
	}

	public static IPath getTemplateStatePath() {
		Bundle b = Platform.getBundle(PLUGIN_ID);
		return Platform.getStateLocation(b);
	}

	static public void log(Exception ex) {
		getDefault().getLog().log(new Status(Status.ERROR, PLUGIN_ID, Status.OK, "No message", ex)); //$NON-NLS-1$
	}
}