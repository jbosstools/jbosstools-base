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
package org.jboss.tools.common.jdt.debug.ui;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;
import org.jboss.tools.common.jdt.debug.VmModel;
import org.jboss.tools.common.jdt.debug.ui.internal.SelectionUtil;
import org.jboss.tools.common.jdt.debug.ui.launching.RemoteJavaApplicationLaunchShortcut;
import org.jboss.tools.common.jdt.debug.ui.preferences.RemoteDebug;
import org.jboss.tools.foundation.ui.plugin.BaseUIPlugin;
import org.osgi.framework.BundleContext;
import org.osgi.service.prefs.BackingStoreException;

/**
 * The activator class controls the plug-in life cycle
 */
public class RemoteDebugUIActivator extends BaseUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.jboss.tools.common.jdt.debug.ui"; //$NON-NLS-1$
	public static final String PORT = "port";
	public static final String DESCRIPTION = "description";
	public static final String SHOW = "show";
	public static final int KEYS = 10;
	public static final String DEFAULT_PORT = "0";
	public static final String DEFAULT_DESCRIPTION = "";
	public static final boolean DEFAULT_SHOW = true;
	public static final String DISCOVER_REMOTE_APPLICATION = "discoverRemoteApplication";
	public static final boolean DEFAULT_DISCOVER_REMOTE_APPLICATION = true;
	public static final String COMMAND_PREFIX = "org.jboss.tools.common.jdt.debug.";
	public static final String CONFIGURE_ACTION_ID = "org.jboss.tools.common.jdt.debug.ui.configure"; //$NON-NLS-1$
	
	public static final String AUTO_CONNECT = "autoConnect";
	public static final boolean AUTO_CONNECT_DEFAULT = false;
	
	private static final String KEY_SEQUENCE_PREFIX = "M1+D ";
	public static final String DISCOVER_REMOTE_APPLICATION_ACTION_ID = "org.jboss.tools.common.jdt.debug.ui.discover";
	
	public static final String REMOTE_DEBUG_PREFERENCE_PAGE_ID = "org.jboss.tools.common.jdt.debug.ui.preferences.RemoteDebugPreferencePage";
	// The shared instance
	private static RemoteDebugUIActivator plugin;
	private static VmModel[] vmModels;
	private static Job discoverRemoteApplicationJob;
	
	/**
	 * The constructor
	 */
	public RemoteDebugUIActivator() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		vmModels = new VmModel[0];
		discoverRemoteApplicationJob = new Job(Messages.Discover_Remote_Applications) {

			@Override
			protected IStatus run(IProgressMonitor monitor) {
				if (monitor.isCanceled()) {
					return Status.CANCEL_STATUS;
				}
				discoverRemoteApplication(monitor);
				monitor.done();
				return Status.OK_STATUS;
			}
		};
		discoverRemoteApplicationJob.setUser(true);
		discoverRemoteApplicationJob.setSystem(false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		vmModels = null;
		super.stop(context);
	}


	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static RemoteDebugUIActivator getDefault() {
		return plugin;
	}
	
	public synchronized VmModel[] getDebugModels(IProgressMonitor monitor) {
		if (isDiscoverRemoteApplication()) {
			VmModel[] newModels = RemoteDebugActivator.getDefault().getDebugModels(monitor);
			if (newModels != null) {
				vmModels = newModels;
			}
		}
		return vmModels;
	}
	
	public synchronized VmModel[] getCurrentDebugModels() {
		return vmModels;
	}
	
	public synchronized void discoverRemoteApplication(IProgressMonitor monitor) {
		VmModel[] newModels = RemoteDebugActivator.getDefault().getDebugModels(monitor);
		if (newModels != null) {
			vmModels = newModels;
		}
	}
	
	public Job discoverRemoteApplicationInJob() {
		Job job = new Job(Messages.Discover_Remote_Applications) {

			@Override
			protected IStatus run(IProgressMonitor monitor) {
				if (monitor.isCanceled()) {
					return Status.CANCEL_STATUS;
				}
				discoverRemoteApplication(monitor);
				monitor.done();
				return Status.OK_STATUS;
			}
		};
		job.setUser(true);
		job.setSystem(false);
		job.schedule(50);
		return job;
	}

	public Job getRemoteApplicationJob() {
		return discoverRemoteApplicationJob;
	}
	
	public static void log(Exception e, String message) {
		IStatus status = new Status(IStatus.ERROR, PLUGIN_ID, message, e);
		plugin.getLog().log(status);
	}

	public static void logWarning(String message) {
		IStatus status = new Status(IStatus.WARNING, PLUGIN_ID, message);
		plugin.getLog().log(status);
	}
	
	public static void log(Throwable e) {
		IStatus status = new Status(IStatus.ERROR, PLUGIN_ID, e
				.getLocalizedMessage(), e);
		plugin.getLog().log(status);
	}

	public static String getPortPreferenceName(int i) {
		return PORT + i;
	}
	
	public static String getDescriptionPreferenceName(int i) {
		return DESCRIPTION + i;
	}
	
	public static String getShowPreferenceName(int i) {
		return SHOW + i;
	}

	public void savePreferences() {
		IEclipsePreferences prefs = getPreferences();
		try {
			prefs.flush();
		} catch (BackingStoreException e) {
			log(e);
		}
	}

	public IEclipsePreferences getPreferences() {
		IEclipsePreferences prefs = InstanceScope.INSTANCE.getNode(PLUGIN_ID);
		return prefs;
	}
	
	public RemoteDebug[] getRemoteDebugs() {
		int keys = RemoteDebugUIActivator.KEYS;
		RemoteDebug[] remoteDebugs = new RemoteDebug[keys];
		IEclipsePreferences preferences = getPreferences();
		for (int i = 0; i < keys; i++) {
			String description = preferences.get(RemoteDebugUIActivator.getDescriptionPreferenceName(i), RemoteDebugUIActivator.DEFAULT_DESCRIPTION);
			String port = preferences.get(RemoteDebugUIActivator.getPortPreferenceName(i), RemoteDebugUIActivator.DEFAULT_PORT);
			boolean show = preferences.getBoolean(RemoteDebugUIActivator.getShowPreferenceName(i), RemoteDebugUIActivator.DEFAULT_SHOW);
			String key = KEY_SEQUENCE_PREFIX + i;
			remoteDebugs[i] = new RemoteDebug(i, key, description, port, show);
		}
		return remoteDebugs;
	}
	
	public List<RemoteDebug> getValidRemoteDebugs() {
		int keys = RemoteDebugUIActivator.KEYS;
		List <RemoteDebug> remoteDebugs = new ArrayList<RemoteDebug>();
		IEclipsePreferences preferences = getPreferences();
		for (int i = 0; i < keys; i++) {
			String description = preferences.get(RemoteDebugUIActivator.getDescriptionPreferenceName(i), RemoteDebugUIActivator.DEFAULT_DESCRIPTION);
			String port = preferences.get(RemoteDebugUIActivator.getPortPreferenceName(i), RemoteDebugUIActivator.DEFAULT_PORT);
			boolean show = preferences.getBoolean(RemoteDebugUIActivator.getShowPreferenceName(i), RemoteDebugUIActivator.DEFAULT_SHOW);
			String key = KEY_SEQUENCE_PREFIX + i;
			RemoteDebug remoteDebug = new RemoteDebug(i, key, description, port, show);
			if (remoteDebug.isValid()) {
				remoteDebugs.add(remoteDebug);
			}
		}
		return remoteDebugs;
	}
	
	public boolean isAutoConnect() {
		return getPreferences().getBoolean(AUTO_CONNECT, AUTO_CONNECT_DEFAULT);
	}
	
	public void setAutoConnect(boolean autoConnect) {
		getPreferences().putBoolean(AUTO_CONNECT, autoConnect);
	}
	
	public boolean isDiscoverRemoteApplication() {
		return getPreferences().getBoolean(DISCOVER_REMOTE_APPLICATION, DEFAULT_DISCOVER_REMOTE_APPLICATION);
	}
	
	public void setDiscoverRemoteApplication(boolean value) {
		getPreferences().putBoolean(DISCOVER_REMOTE_APPLICATION, value);
	}
	
	public void addSelectedProjects(ILaunchConfigurationWorkingCopy wc, ISelection selection,
			IJavaProject javaProject) throws CoreException {
		IJavaElement[] sel2 = SelectionUtil.getJavaElements(selection);
		RemoteDebugActivator.configureSourceLookup(wc, sel2, javaProject);
	}
	
	
	public static void displayLaunchError(Shell shell, ILaunchConfiguration config, Exception e) {
		//String configName = config.getName();
		String title = "Problem Occured"; 
		String message = "Opening the configuration dialog has encoutered a problem.\n\n" + e.getLocalizedMessage();
		MessageDialog.openError(shell, title, message);
	}

	public static RemoteDebug findRemoteDebug(List<RemoteDebug> remoteDebugs,
			String port) {
		for (RemoteDebug remoteDebug:remoteDebugs) {
			if (port != null && port.equals(remoteDebug.getPort())) {
				return remoteDebug;
			}
		}
		return null;
	}
	
	public static ILaunchManager getLaunchManager() {
		return DebugPlugin.getDefault().getLaunchManager();
	}

	public static ILaunchConfigurationType getRemoteJavaApplicationConfigurationType() {
		return RemoteDebugActivator.getRemoteJavaApplicationConfigurationType();
	}
	
	
	public static void openLaunchDebuggerDialog() {
		new RemoteJavaApplicationLaunchShortcut().launch(new StructuredSelection(), null);
	}

}
