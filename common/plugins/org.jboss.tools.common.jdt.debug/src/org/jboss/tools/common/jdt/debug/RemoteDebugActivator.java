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
package org.jboss.tools.common.jdt.debug;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Preferences.IPropertyChangeListener;
import org.eclipse.core.runtime.Preferences.PropertyChangeEvent;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.internal.launching.SocketAttachConnector;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMConnector;
import org.eclipse.jdt.launching.JavaRuntime;
import org.jboss.tools.common.jdt.debug.internal.RemoteDebugLaunchUtil;
import org.jboss.tools.common.jdt.debug.internal.SourceLookupUtil;
import org.jboss.tools.common.jdt.debug.internal.VmModelCache;
import org.jboss.tools.common.jdt.debug.sourcelookup.DebugLaunchConfigurationListener;
import org.jboss.tools.common.jdt.debug.sourcelookup.RemoteDebugSourcePathComputer;
import org.jboss.tools.common.jdt.debug.tools.ToolsCore;
import org.jboss.tools.common.jdt.debug.tools.ToolsCoreException;
import org.jboss.tools.common.jdt.debug.tools.internal.Tools;
import org.jboss.tools.foundation.core.plugin.BaseCorePlugin;
import org.jboss.tools.foundation.core.plugin.log.IPluginLog;
import org.jboss.tools.foundation.core.plugin.log.StatusFactory;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

public class RemoteDebugActivator extends BaseCorePlugin {
	public static final String PLUGIN_ID = "org.jboss.tools.common.jdt.debug"; //$NON-NLS-1$
	
	public static final String UNKNOWN = "<Unknown>";
	public static final String LOCALHOST = "localhost"; //$NON-NLS-1$
	public static final String LAUNCH_CONFIGURATION_ID = "org.jboss.tools.common.jdt.debug.launching.JBossRemoteJavaApplication"; //$NON-NLS-1$
	public static final String REMOTE_JAVA_APPLICATION_ID = IJavaLaunchConfigurationConstants.ID_REMOTE_JAVA_APPLICATION;
	public static final String JBOSS_REMOTE_JAVA_APPLICATION = "JBossRemoteJavaApplication"; //$NON-NLS-1$
	public static final String JBOSS_SOURCE_PATH_COMPUTER_ID = "org.jboss.tools.common.jdt.debug.sourceLookup.remoteDebugSourcePathComputer"; //$NON-NLS-1$
	
	public static final String DT_SOCKET = "dt_socket"; //$NON-NLS-1$
	public static final String LAUNCH_CATEGORY = "org.eclipse.debug.ui.launchGroup.debug"; //$NON-NLS-1$
	
	public static final String ATTR_SELECTED_PROJECTS = "selectedProjects";  //$NON-NLS-1$
	public static final String MAVEN_SOURCEPATH_PROVIDER = "org.eclipse.m2e.launchconfig.sourcepathProvider"; //$NON-NLS-1$
	public static final String MAVEN_CLASSPATH_PROVIDER = "org.eclipse.m2e.launchconfig.classpathProvider"; //$NON-NLS-1$


	
	private static final String MAVEN_PLUGIN_ID = "org.eclipse.m2e.core"; //$NON-NLS-1$
	public static final String MAVEN_NATURE = MAVEN_PLUGIN_ID + ".maven2Nature"; //$NON-NLS-1$
	
	@Deprecated
	public static final String SET_AS_DEFAULT = IPropertyKeys.SET_AS_DEFAULT;
	@Deprecated
	public static final String JDT_JAVA_APPLICATION = RemoteDebugSourcePathComputer.JDT_JAVA_APPLICATION;
	@Deprecated
	public static final String JDT_JUNIT_TEST = RemoteDebugSourcePathComputer.JDT_JUNIT_TEST;
	
	@Deprecated // Should not be public; replacement is private in RemoteDebugLaunchUtil.DEFAULT_REMOTE_JBOSS_APP
	public static final String DEFAULT_REMOTE_JBOSS_APP = "Remote myapp"; //$NON-NLS-1$
	
	@Deprecated // Should not be public; replacement is private in RemoteDebugLaunchUtil.JBOSS_TEMP_JAVA_APPLICATION
	public static final String JBOSS_TEMP_JAVA_APPLICATION = "jbossTempJavaApplication"; //$NON-NLS-1$
	
	private static RemoteDebugActivator plugin;

	private static BundleContext context;
	
	private DebugLaunchConfigurationListener listener;

	static BundleContext getContext() {
		return context;
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext bundleContext) throws Exception {
		RemoteDebugActivator.context = bundleContext;
		plugin = this;
		listener= new DebugLaunchConfigurationListener();
		DebugPlugin.getDefault().getLaunchManager().addLaunchConfigurationListener(listener);
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext bundleContext) throws Exception {
		RemoteDebugActivator.context = null;
		DebugPlugin.getDefault().getLaunchManager().removeLaunchConfigurationListener(listener);
	}
	
	public static RemoteDebugActivator getDefault() {
		return plugin;
	}
	
	private List<VmModel> getVmModels(String hostname, IProgressMonitor monitor) {
		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}
		List<VmModel> models = new ArrayList<VmModel>();
		try {
			Set<Integer> vmPids = ToolsCore.getActiveVmPids(hostname);
        	int i = 1;
    		int size = vmPids.size();
    		monitor.beginTask("Discovering Remote Aplications", vmPids.size() + 1);
    		for (Integer vmPid : vmPids) {
    			monitor.worked(1);
    			monitor.setTaskName(i++ + " out of " + size + ": Discovering port, main class and arguments for process id " + vmPid);
    			VmModel model = getVmModel(hostname, vmPid, true, monitor); 
    			if (model != null) {
    				models.add(model);
    			}
        	}
		} catch(ToolsCoreException tce) {
			pluginLog().logError(tce);
		}
		return models;
	}

	
	public VmModel getCachedVmModel(String hostname, Integer vmPid) {
		return VmModelCache.getDefault().getModel(hostname, vmPid);
	}
	
	public VmModel getVmModel(String hostname, Integer vmPid, IProgressMonitor monitor) {
		VmModel model = getVmModelsUsingTools(hostname, vmPid, monitor);
		VmModelCache.getDefault().cacheModel(hostname, vmPid.intValue(), model);
		return model;
	}

	public VmModel getVmModel(String hostname, Integer vmPid, boolean useCommand, IProgressMonitor monitor) {
		return getVmModel(hostname, vmPid, monitor);
	}

	private VmModel getVmModelsUsingTools(String hostname, Integer vmPid, IProgressMonitor monitor) {
		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}
		if (monitor.isCanceled()) {
			return null;
		}
		boolean monitorable = false;
		try {
			monitorable = ToolsCore.processIsMonitorable(hostname, vmPid);
		} catch(ToolsCoreException c ) {
			pluginLog().logWarning(c);
		}
		if( monitorable ) {
			VmModel model = new VmModel();
			model.setPid(String.valueOf(vmPid));
			// If process is suspended (launched with suspend=y waiting for debugger) these may fail...
			try {
				
				model.setJvmArgs(ToolsCore.getJvmArgs(hostname, vmPid));
				model.setMainClass(ToolsCore.getMainClass(hostname, vmPid));
				model.setMainArgs(ToolsCore.getMainArgs(hostname, vmPid));
			} catch (ToolsCoreException e) {
				// Ignore, expected in case of suspended vm
			}
			return model;
		}
		return null;
	}
	
	public VmModel getDebugModel(String hostname, int pid, boolean useCommand, IProgressMonitor monitor) {
		VmModel model = getVmModel(hostname, pid, useCommand, monitor);
		if( model != null && isDebugModel(model)) {
			return model;
		}
		return null;
	}
	
	public boolean isDebugModel(VmModel model) {
		if (model != null && model.getDebugPort() != null && DT_SOCKET.equals(model.getTransport())) {
			return true;
		}
		return false;
	}
	
	public VmModel[] getDebugModels(String hostname, IProgressMonitor monitor) {
		List<VmModel> debugModels = new ArrayList<VmModel>();
		List<VmModel> models = getVmModels(hostname, monitor);
		if (models == null) {
			return null;
		}
		for (VmModel model:models) {
			if (isDebugModel(model)) {
				debugModels.add(model);
			}
		}
		return debugModels.toArray(new VmModel[0]);
	}

	
	/**
	 * Returns whether a jdk has been found and set from which
	 * the proper sun classes can be accessed
	 * @return
	 */
	public boolean isJdk() {
		return Tools.getInstance().isReady();
	}
	

	public static IVMConnector getDefaultVMConnector() {
		IVMConnector[] connectors = JavaRuntime.getVMConnectors();
		for (IVMConnector connector:connectors) {
			if (connector instanceof SocketAttachConnector) {
				return connector;
			}
		}
		return null;
	}

	public VmModel[] getDebugModels(IProgressMonitor monitor) {
		return getDebugModels(LOCALHOST, monitor);
	}

	public static boolean m2eExists() {
		Bundle bundle = Platform.getBundle(MAVEN_PLUGIN_ID);
		return bundle != null;
	}
	
	
	/*
	 * Methods related to launch configs
	 */
	
	public ILaunchConfiguration getDefaultLaunchConfiguration() {
		return RemoteDebugLaunchUtil.getDefaultLaunchConfiguration();
	}

	public ILaunchConfiguration[] getLaunchConfigurations() {
		return RemoteDebugLaunchUtil.getLaunchConfigurations();
	}
	
	public static boolean isRemoteDebuggerConnected(String host, int port) {
		return getExistingRemoteDebugLaunch(host,port) != null;
	}
	public static ILaunch getExistingRemoteDebugLaunch(String host, int port) {
		return RemoteDebugLaunchUtil.getExistingRemoteDebugLaunch(host, port);
	}
	
	public static ILaunchConfiguration createTemporaryLaunchConfiguration(String projectName, String typeId)
			throws CoreException {
		return RemoteDebugLaunchUtil.createTemporaryLaunchConfiguration(projectName, typeId);
	}
	
	public static ILaunchConfigurationWorkingCopy createNewLaunchConfiguration(ILaunchConfigurationType type)
			throws CoreException {
		return RemoteDebugLaunchUtil.createNewLaunchConfiguration(type);
	}
	
	public static ILaunchConfigurationType getRemoteJavaApplicationConfigurationType() {
		return RemoteDebugLaunchUtil.getRemoteJavaApplicationConfigurationType();
	}
	
	public static ILaunchConfiguration createOrGetDefaultLaunchConfiguration(String port, String host, IJavaProject javaProject, IJavaElement[] selection) throws CoreException {
		return RemoteDebugLaunchUtil.createOrGetDefaultLaunchConfiguration(port, host, javaProject, selection);
	}
	
	public static void configureSourceLookup(ILaunchConfigurationWorkingCopy wc, 
			IJavaElement[] selection, IJavaProject javaProject) throws CoreException {
		new SourceLookupUtil().addSelectedProjects(wc, selection, javaProject);
	}
	/**
	 * Get the IPluginLog for this plugin. This method 
	 * helps to make logging easier, for example:
	 * 
	 *     FoundationCorePlugin.pluginLog().logError(etc)
	 *  
	 * @return IPluginLog object
	 */
	public static IPluginLog pluginLog() {
		return getDefault().pluginLogInternal();
	}

	/**
	 * Get a status factory for this plugin
	 * @return status factory
	 */
	public static StatusFactory statusFactory() {
		return getDefault().statusFactoryInternal();
	}
}
