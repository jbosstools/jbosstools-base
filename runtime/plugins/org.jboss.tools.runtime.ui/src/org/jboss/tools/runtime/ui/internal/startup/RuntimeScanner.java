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
package org.jboss.tools.runtime.ui.internal.startup;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.runtime.core.model.RuntimeDefinition;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.util.RuntimeInitializerUtil;
import org.jboss.tools.runtime.core.util.RuntimeModelUtil;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;
import org.jboss.tools.runtime.ui.internal.Messages;
import org.jboss.tools.runtime.ui.internal.dialogs.SearchRuntimePathDialog;
import org.jboss.tools.runtime.ui.internal.preferences.JBossRuntimePreferencesInitializer;

/**
 * @author snjeza
 * 
 */
public class RuntimeScanner implements IStartup {

	@Override
	public void earlyStartup() {
		if( Boolean.getBoolean("skip.runtime.scanner"))  //$NON-NLS-1$
			return;
		
		final boolean firstStart = isFirstStart();
		Job runtimeJob = new Job(Messages.RuntimeScanner_Searching_runtimes) {
			
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				if (firstStart) {
					JBossRuntimeStartup.initializeRuntimes(monitor);
				}
				boolean openDialog = wouldOpenSearchRuntimePathDialog(firstStart, monitor);
				if (monitor.isCanceled()) {
					RuntimeUIActivator.getDefault().getModel().setRuntimePaths(null);
					return Status.CANCEL_STATUS;
				}
				if (openDialog) {
					Display.getDefault().asyncExec(new Runnable() {
						public void run() {
							Shell shell = PlatformUI.getWorkbench().getModalDialogShellProvider().getShell();
							Set<RuntimePath> runtimePaths = new HashSet<RuntimePath>();
							for (RuntimePath runtimePath:RuntimeUIActivator.getDefault().getModel().getRuntimePaths()) {
								if (runtimePath.isScanOnEveryStartup() || firstStart) {
									runtimePaths.add(runtimePath);
								}
							}
							RuntimePath[] asArr = runtimePaths.toArray(new RuntimePath[runtimePaths.size()]);
							if (runtimePaths.size() > 0) {
								SearchRuntimePathDialog.launchSearchRuntimePathDialog(
										shell, asArr, false, 7);
							}
						}
					});
				}
				return Status.OK_STATUS;
			}
		};
		runtimeJob.setUser(false);
		runtimeJob.setSystem(false);
		runtimeJob.setPriority(Job.LONG);
		runtimeJob.schedule(1000);
		
		setFirstStartFalse();
	}

	
	private boolean isFirstStart() {
		boolean firstStartWorkspace = RuntimeUIActivator.getDefault().
				getPreferenceStore().getBoolean(JBossRuntimePreferencesInitializer.FIRST_START);
		IEclipsePreferences configurationNode = ConfigurationScope.INSTANCE.getNode(RuntimeUIActivator.PLUGIN_ID);
		boolean firstStartConfiguration =	configurationNode.getBoolean(JBossRuntimePreferencesInitializer.FIRST_START, true);
		final boolean firstStart = firstStartWorkspace || firstStartConfiguration;
		return firstStart;
	}
	
	private void setFirstStartFalse() {
		IEclipsePreferences configurationNode = ConfigurationScope.INSTANCE.getNode(RuntimeUIActivator.PLUGIN_ID);
		configurationNode.putBoolean(JBossRuntimePreferencesInitializer.FIRST_START, false);
		RuntimeUIActivator.getDefault().getPreferenceStore().setValue(JBossRuntimePreferencesInitializer.FIRST_START, false);
	}
	
	private boolean wouldOpenSearchRuntimePathDialog(boolean firstStart, IProgressMonitor monitor) {
		RuntimePath[] runtimePaths = RuntimeUIActivator.getDefault().getModel().getRuntimePaths();
		for (RuntimePath runtimePath:runtimePaths) {
			if (!firstStart && !runtimePath.isScanOnEveryStartup()) {
				continue;
			}
			if (monitor.isCanceled()) {
				return false;
			}
			if (runtimePath.isModified()) {
				RuntimeInitializerUtil.createRuntimeDefinitions(runtimePath, monitor);
				RuntimeModelUtil.updateTimestamps(runtimePaths);
			}
			monitor.setTaskName(Messages.RuntimeScanner_JBoss_Runtime_Detector_checking + runtimePath.getPath());
			RuntimeDefinition[] runtimeDefinitions = runtimePath.getRuntimeDefinitions();
			for (RuntimeDefinition runtimeDefinition:runtimeDefinitions) {
				if (monitor.isCanceled()) {
					return false;
				}
				if (!runtimeDefinition.isEnabled()) {
					continue;
				}
				monitor.setTaskName(Messages.RuntimeScanner_JBoss_Runtime_Detector_checking + runtimeDefinition.getLocation());
				if (!RuntimeModelUtil.verifyRuntimeDefinitionCreated(runtimeDefinition)) {
					return true;
				}
			}
		}
		return false;
	}

}
