/*************************************************************************************
 * Copyright (c) 2010-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimeModel;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.util.RuntimeInitializerUtil;
import org.jboss.tools.runtime.core.util.RuntimeModelUtil;
import org.jboss.tools.runtime.ui.dialogs.SearchRuntimePathDialog;
import org.jboss.tools.runtime.ui.download.DownloadRuntimes;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 * 
 * @author snjeza
 * 
 */
public class RuntimeUIActivator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.jboss.tools.runtime.ui"; //$NON-NLS-1$

	// The shared instance
	private static RuntimeUIActivator plugin;

	private static IEclipsePreferences prefs;

	public static final String LASTPATH = "lastPath";

	public static final String RUNTIME_PATHS = "runtimePaths";

	public static final String PATH = "path";

	public static final String RUNTIME_PATH = "runtimePath";

	public static final String SCAN_ON_EVERY_STAERTUP = "scanOnEveryStartup";

	public static final String FIRST_START = "firstStart"; //$NON-NLS-1$

	public static final String PREFERENCES_VERSION = "version"; //$NON-NLS-1$

	private BundleContext context;
	
	private RuntimeModel runtimeModel;
	
	/**
	 * The constructor
	 */
	public RuntimeUIActivator() {
	}

	public BundleContext getBundleContext() {
		return context;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		this.context = context;
		RuntimeCoreActivator.getDefault().setDownloader(new DownloadRuntimes());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		RuntimeCoreActivator.getDefault().setDownloader(null);
		saveRuntimePreferences();
		runtimeModel = null;
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static RuntimeUIActivator getDefault() {
		return plugin;
	}

	public RuntimeModel getModel() {
		if (runtimeModel == null) {
			runtimeModel = new RuntimeModel(getPreferences());
		}
		return runtimeModel;
	}
	
	public static void log(Throwable e) {
		IStatus status = new Status(IStatus.ERROR, PLUGIN_ID, e.getLocalizedMessage(), e);
		RuntimeUIActivator.getDefault().getLog().log(status);
	}
	
	public static void log(Throwable e, String message) {
		IStatus status = new Status(IStatus.ERROR, PLUGIN_ID, message, e);
		RuntimeUIActivator.getDefault().getLog().log(status);
	}
	
	public static RuntimeCheckboxTreeViewer createRuntimeViewer(final RuntimePath[] runtimePaths2, Composite composite, int heightHint) {
		return new RuntimeCheckboxTreeViewer(composite, runtimePaths2, heightHint);
	}

	public static RuntimeCheckboxTreeViewer createRuntimeViewer(final Set<RuntimePath> runtimePaths2, Composite composite, int heightHint) {
		return new RuntimeCheckboxTreeViewer(composite, runtimePaths2, heightHint);
	}

	public static SearchRuntimePathDialog launchSearchRuntimePathDialog(Shell shell, 
			final RuntimePath[] runtimePaths, boolean needRefresh, int heightHint) {
		IRunnableWithProgress op = new IRunnableWithProgress() {
			public void run(IProgressMonitor monitor) 
					throws InvocationTargetException, InterruptedException {
				RuntimeInitializerUtil.createRuntimeDefinitions(runtimePaths, monitor);
			}
		};
		try {
			HashSet<RuntimePath> set = new HashSet<RuntimePath>(Arrays.asList(runtimePaths));
			
			SearchRuntimePathDialog dialog = new SearchRuntimePathDialog(shell, set, needRefresh, heightHint);
			dialog.run(true, true, op);
			return dialog;
		} catch (InvocationTargetException e1) {
			RuntimeUIActivator.log(e1);
		} catch (InterruptedException e1) {
			// ignore
		}
		return null;
	}
	
	public static void refreshRuntimes(Shell shell, final RuntimePath[] runtimePaths, 
			final RuntimeCheckboxTreeViewer viewer, boolean needRefresh) {
		SearchRuntimePathDialog dialog = launchSearchRuntimePathDialog(
				shell, runtimePaths, needRefresh, 15);
		if (viewer != null) {
			dialog.getShell().addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent e) {
					viewer.updateInput(runtimePaths);
				}
			});
		}
	}

	public static boolean runtimeExists(RuntimeDefinition runtimeDefinition) {
		return RuntimeModelUtil.verifyRuntimeDefinitionCreated(runtimeDefinition, false);
	}
	
	public void saveRuntimePreferences() {
		getModel().saveRuntimePaths();
		RuntimeCoreActivator.getDefault().saveEnabledDetectors();
	}

	private static IEclipsePreferences getPreferences() {
		if (prefs == null) {
			prefs = ConfigurationScope.INSTANCE.getNode(PLUGIN_ID);
		}
		return prefs;
	}
	
	public RuntimeSharedImages getSharedImages() {
		return RuntimeSharedImages.getDefault();
	}
	
	/* These are all deprecated and shouldn't be used */
	@Deprecated
	public List<RuntimeDefinition> getServerDefinitions() {
		return RuntimeModelUtil.getRuntimeDefinitions(getModel().getRuntimePaths());
	}

	@Deprecated
	public Set<IRuntimeDetector> getRuntimeDetectors() {
		return RuntimeCoreActivator.getDefault().getRuntimeDetectors();
	}

	@Deprecated
	public static void setTimestamp(RuntimePath[] runtimePaths2) {
		RuntimeModelUtil.updateTimestamps(runtimePaths2);
	}

	@Deprecated
	public static boolean runtimeCreated(RuntimeDefinition runtimeDefinition) {
		return RuntimeModelUtil.verifyRuntimeDefinitionCreated(runtimeDefinition);
	}
	
	@Deprecated
	public static RuntimePath[] getRuntimePaths() {
		return getDefault().getModel().getRuntimePaths();
	}
	
}
