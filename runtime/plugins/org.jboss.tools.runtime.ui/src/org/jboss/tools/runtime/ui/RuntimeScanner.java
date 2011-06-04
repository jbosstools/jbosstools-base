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

import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.runtime.core.JBossRuntimeLocator;
import org.jboss.tools.runtime.core.model.RuntimePath;
import org.jboss.tools.runtime.core.model.ServerDefinition;

/**
 * @author snjeza
 * 
 */
public class RuntimeScanner implements IStartup {

	@Override
	public void earlyStartup() {
		String skipRuntimeScanner = System.getProperty("skip.runtime.scanner", "false");
		if ("true".equals(skipRuntimeScanner)) {
			return;
		}
		final boolean firstStart = RuntimeUIActivator.getDefault().
			getPreferenceStore().getBoolean(RuntimeUIActivator.FIRST_START);
		Job runtimeJob = new Job("Searching runtimes...") {
			
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				boolean exists = runtimeExists(firstStart, monitor);
				if (monitor.isCanceled()) {
					RuntimeUIActivator.getDefault().refreshRuntimePreferences();
					return Status.CANCEL_STATUS;
				}
				if (exists) {
					Display.getDefault().asyncExec(new Runnable() {
						public void run() {
							Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
							RuntimeUIActivator.refreshRuntimes(shell, RuntimeUIActivator.getDefault().getRuntimePaths(), null, false, 7);
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
		
		RuntimeUIActivator.getDefault().getPreferenceStore().setValue(RuntimeUIActivator.FIRST_START, false);
	}

	private boolean runtimeExists(boolean firstStart, IProgressMonitor monitor) {
		List<RuntimePath> runtimePaths = RuntimeUIActivator.getDefault().getRuntimePaths();
		for (RuntimePath runtimePath:runtimePaths) {
			if (!firstStart && !runtimePath.isScanOnEveryStartup()) {
				continue;
			}
			if (monitor.isCanceled()) {
				return false;
			}
			if (runtimePath.isModified()) {
				JBossRuntimeLocator locator = new JBossRuntimeLocator();
				List<ServerDefinition> serverDefinitions = locator.searchForRuntimes(runtimePath.getPath(), monitor);
				if (monitor.isCanceled()) {
					return false;
				}
				runtimePath.getServerDefinitions().clear();
				for (ServerDefinition serverDefinition:serverDefinitions) {
					serverDefinition.setRuntimePath(runtimePath);
				}
				runtimePath.getServerDefinitions().addAll(serverDefinitions);
				RuntimeUIActivator.setTimestamp(runtimePaths);
			}
			monitor.setTaskName("JBoss Runtime Detector: checking " + runtimePath.getPath());
			List<ServerDefinition> serverDefinitions = runtimePath.getServerDefinitions();
			for (ServerDefinition serverDefinition:serverDefinitions) {
				if (monitor.isCanceled()) {
					return false;
				}
				if (!serverDefinition.isEnabled()) {
					continue;
				}
				monitor.setTaskName("JBoss Runtime Detector: checking " + serverDefinition.getLocation());
				if (!RuntimeUIActivator.runtimeCreated(serverDefinition)) {
					return true;
				}
			}
		}
		return false;
	}

}
