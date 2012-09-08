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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.IRuntimeDetector;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.util.RuntimeInitializerUtil;
import org.jboss.tools.runtime.core.util.RuntimeModelUtil;
import org.jboss.tools.runtime.core.util.RuntimePathPreferenceIO;
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
	
	private Set<RuntimePath> runtimePaths = new HashSet<RuntimePath>();
	
	private List<RuntimeDefinition> runtimeDefinitions;
	
	private ListenerList runtimePathChangeChangeListeners;
	

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
		runtimePaths = null;
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

	public static void log(Throwable e) {
		IStatus status = new Status(IStatus.ERROR, PLUGIN_ID, e
				.getLocalizedMessage(), e);
		RuntimeUIActivator.getDefault().getLog().log(status);
	}
	
	public static void log(Throwable e, String message) {
		IStatus status = new Status(IStatus.ERROR, PLUGIN_ID, message, e);
		RuntimeUIActivator.getDefault().getLog().log(status);
	}
	
	public static CheckboxTreeViewer createRuntimeViewer(final Set<RuntimePath> runtimePaths2, Composite composite, int heightHint) {
		return new RuntimeCheckboxTreeViewer(composite, runtimePaths2, heightHint);
	}
	
	public static void refreshRuntimes(Shell shell, final Set<RuntimePath> runtimePaths, final CheckboxTreeViewer viewer, boolean needRefresh, int heightHint) {
		IRunnableWithProgress op = new IRunnableWithProgress() {
			public void run(IProgressMonitor monitor) 
					throws InvocationTargetException, InterruptedException {
				RuntimeInitializerUtil.createRuntimeDefinitions(runtimePaths, monitor);
			}
		};
		try {
			SearchRuntimePathDialog dialog = new SearchRuntimePathDialog(shell, runtimePaths, needRefresh, heightHint);
			dialog.run(true, true, op);
			if (viewer != null) {
				dialog.getShell().addDisposeListener(new DisposeListener() {

					@Override
					public void widgetDisposed(DisposeEvent e) {
						viewer.setInput(null);
						List<RuntimeDefinition> runtimeDefinitions = new ArrayList<RuntimeDefinition>();
						for (RuntimePath runtimePath : runtimePaths) {
							runtimeDefinitions.addAll(runtimePath
									.getRuntimeDefinitions());
							viewer.setInput(runtimeDefinitions);
							for (RuntimeDefinition serverDefinition : runtimeDefinitions) {
								runtimeExists(serverDefinition);
								viewer.setChecked(serverDefinition,
										serverDefinition.isEnabled());
							}
						}
					}
				});
			}
		} catch (InvocationTargetException e1) {
			RuntimeUIActivator.log(e1);
		} catch (InterruptedException e1) {
			// ignore
		}
	}

	public static boolean runtimeExists(RuntimeDefinition runtimeDefinition) {
		return RuntimeModelUtil.verifyRuntimeDefinitionCreated(runtimeDefinition, false);
	}
	
	public void saveRuntimePreferences() {
		saveRuntimePaths();
		RuntimeCoreActivator.getDefault().saveEnabledDetectors();
	}
	
	private void initRuntimePaths() throws WorkbenchException {
		String runtimes = getPreferences().get(RUNTIME_PATHS, null);
		if (runtimes == null || runtimes.isEmpty()) {
			runtimePaths = new HashSet<RuntimePath>();
			return;
		}
		runtimePaths = RuntimePathPreferenceIO.loadRuntimePathsFromPreferenceString(runtimes);
	}

	private static IEclipsePreferences getPreferences() {
		if (prefs == null) {
			prefs = ConfigurationScope.INSTANCE.getNode(PLUGIN_ID);
		}
		return prefs;
	}
	
	public void saveRuntimePaths() {
		if (runtimePaths == null)
			return;
		
		try {
			String runtimes = RuntimePathPreferenceIO.getPreferenceOutputString(runtimePaths);
			getPreferences().put(RUNTIME_PATHS, runtimes);
			getPreferences().flush();
			fireRuntimePathsChanged();
		} catch (Exception e) {
			log(e);
		}
	}

	private void fireRuntimePathsChanged() {
		if (runtimePathChangeChangeListeners != null) {
			Object[] listeners = runtimePathChangeChangeListeners.getListeners();
			for (Object listener:listeners ) {
				IRuntimePathChangeListener runtimePathChangeChangeListener = (IRuntimePathChangeListener) listener;
				runtimePathChangeChangeListener.changed();
			}
		}
	}

	public Set<RuntimePath> getRuntimePaths() {
		if (runtimePaths == null) {
			try {
				initRuntimePaths();
			} catch (WorkbenchException e) {
				log(e);
				runtimePaths = new HashSet<RuntimePath>();
			}
		}
		return runtimePaths;
	}
	
	public List<RuntimeDefinition> getServerDefinitions() {
		if (runtimeDefinitions == null) {
			runtimeDefinitions = new ArrayList<RuntimeDefinition>();
		} else {
			runtimeDefinitions.clear();
		}
		for (RuntimePath runtimePath:getRuntimePaths()) {
			runtimeDefinitions.addAll(runtimePath.getRuntimeDefinitions());
		}
		return runtimeDefinitions;
	}

	public Set<IRuntimeDetector> getRuntimeDetectors() {
		return RuntimeCoreActivator.getDefault().getRuntimeDetectors();
	}

	public void initDefaultRuntimePreferences() {
		runtimePaths = new HashSet<RuntimePath>();
	}
	
	public static void setTimestamp(Set<RuntimePath> runtimePaths2) {
		RuntimeModelUtil.updateTimestamps(runtimePaths2);
	}

	public void refreshRuntimePreferences() {
		runtimePaths = null;
	}
	
	public static boolean runtimeCreated(RuntimeDefinition runtimeDefinition) {
		return RuntimeModelUtil.verifyRuntimeDefinitionCreated(runtimeDefinition);
	}
	
	public void addRuntimePathChangeListener(IRuntimePathChangeListener listener) {
		if (runtimePathChangeChangeListeners == null)
			runtimePathChangeChangeListeners = new ListenerList();
		runtimePathChangeChangeListeners.add(listener);
	}
	
	public void removeRuntimePathChangeListener(IRuntimePathChangeListener listener) {
		if (runtimePathChangeChangeListeners == null)
			return;
		runtimePathChangeChangeListeners.remove(listener);
		if (runtimePathChangeChangeListeners.size() == 0)
			runtimePathChangeChangeListeners = null;
	}
}
