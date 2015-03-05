/*************************************************************************************
 * Copyright (c) 2015 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.internal.launching.JavaMigrationDelegate;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.jboss.tools.common.jdt.debug.IPropertyKeys;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;

public class RemoteDebugLaunchUtil {
	
	private static final String DEFAULT_REMOTE_JBOSS_APP = "Remote myapp"; //$NON-NLS-1$
	private static final String JBOSS_TEMP_JAVA_APPLICATION = "jbossTempJavaApplication"; //$NON-NLS-1$
	private static final String REMOTE_JAVA_APP_LAUNCH_TYPE_ID = "org.eclipse.jdt.launching.remoteJavaApplication"; //$NON-NLS-1$
	
	
	/** 
	 * Check if there's an existing and active, remote java app launch for the given host/port
	 * @param host
	 * @param port
	 * @return
	 */
	public static boolean isRemoteDebuggerConnected(String host, int port) {
		return getExistingRemoteDebugLaunch(host, port) != null;
	}
	
	public static ILaunch getExistingRemoteDebugLaunch(String host, int port) {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunch[] all = manager.getLaunches();
		for( int i = 0; i < all.length; i++ ) {
			if( !all[i].isTerminated()) {
				try {
					ILaunchConfiguration lc = all[i].getLaunchConfiguration();
					String type = lc.getType().getIdentifier();
					System.out.println(type);
					if( type.equals(REMOTE_JAVA_APP_LAUNCH_TYPE_ID)) { //$NON-NLS-1$
						Map m = lc.getAttribute(IJavaLaunchConfigurationConstants.ATTR_CONNECT_MAP, (Map)null); //$NON-NLS-1$
						if( m != null ) {
							String h = (String)m.get("hostname");
							String p = (String)m.get("port");
							if( host.equals(h) && Integer.toString(port).equals(p)) {
								return all[i];
							}
						}
					}
				} catch(CoreException ce) {
					// Ignore
				}
			}
		}
		return null;
	}
	
	
	public static ILaunchConfiguration createTemporaryLaunchConfiguration(String projectName, String typeId)
			throws CoreException {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunchConfigurationType type = manager.getLaunchConfigurationType(typeId);
		ILaunchConfigurationWorkingCopy wc = type.newInstance(null, manager.generateLaunchConfigurationName(JBOSS_TEMP_JAVA_APPLICATION));
		wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, projectName);
		ILaunchConfiguration config = wc.doSave();
		return config;
	}
	
	public static ILaunchConfigurationWorkingCopy createNewLaunchConfiguration(ILaunchConfigurationType type)
			throws CoreException {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunchConfigurationWorkingCopy wc = type.newInstance(null, manager.generateLaunchConfigurationName(DEFAULT_REMOTE_JBOSS_APP));
		wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_ALLOW_TERMINATE, false);
		wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_VM_CONNECTOR, RemoteDebugActivator.getDefaultVMConnector().getIdentifier());
		wc.setAttribute(RemoteDebugActivator.JBOSS_REMOTE_JAVA_APPLICATION, true);
		return wc;
	}
	


	public static ILaunchConfiguration[] getLaunchConfigurations() {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunchConfigurationType type = manager.getLaunchConfigurationType(RemoteDebugActivator.REMOTE_JAVA_APPLICATION_ID);
		try {
			ILaunchConfiguration[] configs = manager.getLaunchConfigurations(type);
			List<ILaunchConfiguration> jbossConfigurations = new ArrayList<ILaunchConfiguration>();
			for (ILaunchConfiguration config:configs) {
				if (config.getAttribute(RemoteDebugActivator.JBOSS_REMOTE_JAVA_APPLICATION, false)) {
					jbossConfigurations.add(config);
				}
			}
			return jbossConfigurations.toArray(new ILaunchConfiguration[0]);
		} catch (CoreException e) {
			return null;
		}
	}
	
	
	public static ILaunchConfiguration getDefaultLaunchConfiguration() {
		ILaunchConfiguration[] configs = getLaunchConfigurations();
		if (configs != null && configs.length > 0) {
			for (ILaunchConfiguration config:configs) {
				boolean isDefault = false;
				try {
					isDefault = config.getAttribute(IPropertyKeys.SET_AS_DEFAULT, false);
				} catch (CoreException e) {
					// ignore
				}
				if (isDefault) {
					return config;
				}
			}
			if (configs.length == 1) {
				try {
					ILaunchConfigurationWorkingCopy wc = configs[0].getWorkingCopy();
					wc.setAttribute(IPropertyKeys.SET_AS_DEFAULT, true);
					wc.doSave();
				} catch (CoreException e) {
					RemoteDebugActivator.pluginLog().logError(e);
				}
			}
			return configs[0];
		}
		return null;
	}
	
	

	public static ILaunchConfiguration createOrGetDefaultLaunchConfiguration(String port, String host, IJavaProject javaProject, IJavaElement[] selection) throws CoreException {
		ILaunchConfiguration config = RemoteDebugActivator.getDefault().getDefaultLaunchConfiguration();
		ILaunchConfigurationWorkingCopy wc = null;
		if (config != null) {
			wc = config.getWorkingCopy();
			setAttribute(wc, host, port);
			wc.doSave();
		} else {
			ILaunchConfigurationType configType = getRemoteJavaApplicationConfigurationType();
			wc = createNewLaunchConfiguration(configType);
		}
		if (javaProject != null && javaProject.isOpen()) {
			wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, javaProject.getElementName());

			try {
				JavaMigrationDelegate.updateResourceMapping(wc);
			} catch (CoreException ce) {
				RemoteDebugActivator.pluginLog().logError(ce);
			}
		}
		setAttribute(wc, host, port);
		if (selection != null) {
			RemoteDebugActivator.configureSourceLookup(wc, selection, javaProject);
		}
		config = wc.doSave();
		return config;
	}

	private static void setAttribute(ILaunchConfigurationWorkingCopy wc, String host, String port) {
		Map attrMap = new HashMap(2);
		attrMap.put("hostname", host); //$NON-NLS-1$
		attrMap.put("port", port == null ? "" : port); //$NON-NLS-1$
		wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_CONNECT_MAP, attrMap);
	}
	
	public static ILaunchConfigurationType getRemoteJavaApplicationConfigurationType() {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunchConfigurationType type = manager
				.getLaunchConfigurationType(RemoteDebugActivator.REMOTE_JAVA_APPLICATION_ID);
		return type;
	}
	
}
