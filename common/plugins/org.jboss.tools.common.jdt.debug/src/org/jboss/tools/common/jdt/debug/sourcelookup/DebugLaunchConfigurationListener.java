/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.sourcelookup;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationListener;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.sourcelookup.ISourcePathComputer;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.JavaRuntime;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;

/**
 * 
 * @author snjeza
 *
 */
public class DebugLaunchConfigurationListener implements
		ILaunchConfigurationListener {

	public void launchConfigurationAdded(ILaunchConfiguration configuration) {
		updateLaunchConfiguration(configuration);
	}

	public void launchConfigurationChanged(ILaunchConfiguration configuration) {
		updateLaunchConfiguration(configuration);
	}

	public void launchConfigurationRemoved(ILaunchConfiguration configuration) {
		// do nothing
	}

	private void updateLaunchConfiguration(ILaunchConfiguration configuration) {
		try {
			if (!RemoteDebugActivator.REMOTE_JAVA_APPLICATION_ID
					.equals(configuration.getType().getIdentifier())) {
				return;
			}
			if (configuration.getAttributes().containsKey(
					IJavaLaunchConfigurationConstants.ATTR_CLASSPATH_PROVIDER)) {
				return;
			}
			if (!configuration.getAttribute(RemoteDebugActivator.JBOSS_REMOTE_JAVA_APPLICATION, false)) {
				return;
			}
				
			IJavaProject javaProject = JavaRuntime
					.getJavaProject(configuration);
			if (javaProject == null) {
				return; 
			}
			String sourcePathComputer = configuration.getAttribute(ISourcePathComputer.ATTR_SOURCE_PATH_COMPUTER_ID, (String) null);
			ILaunchConfigurationWorkingCopy wc = getLaunchConfigurationWorkingCopy(configuration);
			boolean save = false;
			if (sourcePathComputer == null) {
				wc.setAttribute(ISourcePathComputer.ATTR_SOURCE_PATH_COMPUTER_ID, RemoteDebugActivator.JBOSS_SOURCE_PATH_COMPUTER_ID);
				save = true;
			}
			if (javaProject.getProject().hasNature(RemoteDebugActivator.MAVEN_NATURE)) {
				if (RemoteDebugActivator.m2eExists()) {
					wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_CLASSPATH_PROVIDER, 
						RemoteDebugActivator.MAVEN_CLASSPATH_PROVIDER);
					wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_SOURCE_PATH_PROVIDER,
			    		RemoteDebugActivator.MAVEN_SOURCEPATH_PROVIDER);
				} else {
					String classPathProvider = wc.getAttribute(IJavaLaunchConfigurationConstants.ATTR_CLASSPATH_PROVIDER, "");
					if (RemoteDebugActivator.MAVEN_CLASSPATH_PROVIDER.equals(classPathProvider)) {
						wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_CLASSPATH_PROVIDER, 
								(String)null);
					}
					String sourcePathProvider = wc.getAttribute(IJavaLaunchConfigurationConstants.ATTR_SOURCE_PATH_PROVIDER, "");
					if (RemoteDebugActivator.MAVEN_SOURCEPATH_PROVIDER.equals(sourcePathProvider)) {
						wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_SOURCE_PATH_PROVIDER, 
								(String)null);
					}
				}
			    save = true;
			}
			if (save) {
				wc.doSave();
			}
		} catch (CoreException e) {
			RemoteDebugActivator.log(e);
		}
	}

	private ILaunchConfigurationWorkingCopy getLaunchConfigurationWorkingCopy(
			ILaunchConfiguration configuration) throws CoreException {
		ILaunchConfigurationWorkingCopy wc;
		if (configuration instanceof ILaunchConfigurationWorkingCopy) {
			wc = (ILaunchConfigurationWorkingCopy) configuration;
		} else {
			wc = configuration.getWorkingCopy();
		}
		return wc;
	}
	
	

}
