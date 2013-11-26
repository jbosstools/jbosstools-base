/*************************************************************************************
 * Copyright (c) 2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui.wizard;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.foundation.core.tasks.TaskModel;
import org.jboss.tools.foundation.ui.xpl.taskwizard.TaskWizard;
import org.jboss.tools.foundation.ui.xpl.taskwizard.WizardFragment;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IDownloadRuntimeFilter;
import org.jboss.tools.runtime.ui.internal.Messages;
import org.jboss.tools.runtime.ui.internal.wizard.DownloadRuntimeLicenseFragment;
import org.jboss.tools.runtime.ui.internal.wizard.FinalizeRuntimeDownloadFragment;
import org.jboss.tools.runtime.ui.internal.wizard.SelectDownloadRuntimeFragment;

/**
 * A public API class representing the DownloadRuntimes wizard.
 * The underlying implementation is of a TaskWizard. 
 * The wizard nominally has 3 pages for the standard case.
 *  
 * DownloadRuntime objects that have an authenticator
 * associated with them are expected to use the 
 * downloadRuntimeAuthenticatorUI extension point 
 * to provide a UI for their authentication efforts, and
 * to eventually add a URL into the task model 
 * for use by the final page of the wizard. 
 * 
 * @Since 3.0
 *
 */
public class DownloadRuntimesTaskWizard extends TaskWizard {
	
	/**
	 * A taskmodel key for a Map<String,DownloadRuntime>  
	 * full of downloadRuntime objects
	 */
	public static final String DL_RUNTIME_MAP = "dl.runtime.map.prop";  //$NON-NLS-1$
	/**
	 * A taskmodel key for accessing the currently-selected
	 * DownloadRuntime object
	 */
	public static final String DL_RUNTIME_PROP = "dl.runtime.prop"; //$NON-NLS-1$

	
	/**
	 * A taskmodel key for accessing the currently-selected
	 * DownloadRuntime object's URL in the event that 
	 * an authenticator WizardFragment has acquired a URL for use.
	 * The value should be of type String
	 */
	public static final String DL_RUNTIME_URL = "dl.runtime.url"; //$NON-NLS-1$

	
	/**
	 * A default constructor
	 */
	public DownloadRuntimesTaskWizard() {
		this( new String[0], new String[0]);
	}
	
	/**
	 * Constructor with a map of DownloadRuntime
	 */
	private DownloadRuntimesTaskWizard(Map<String, DownloadRuntime> downloadRuntime ) {
		this(new String[0], new String[0], downloadRuntime);
	}	
	/**
	 * 
	 * A constructor that accepts a filter to display only a 
	 * subset of the possible DownloadRuntime objects
	 * 
	 * @param filter
	 */
	public DownloadRuntimesTaskWizard(IDownloadRuntimeFilter filter) {
		this(dlRuntimeMapFromFilter(filter));
	}
	
	/**
	 * A constructor to present the wizard with a pre-defined list
	 * of DownloadRuntime objects.
	 * 
	 * @param runtimes
	 */
	public DownloadRuntimesTaskWizard(List<DownloadRuntime> runtimes) {
		this(dlRuntimeMapFromList(runtimes));
	}
	
	/*
	 * Turn a simple list of DownloadRuntime into the Map<String,DownloadRuntime> the framework uses
	 */
	private static Map<String, DownloadRuntime> dlRuntimeMapFromList(List<DownloadRuntime> runtimes) {
		HashMap<String, DownloadRuntime> downloadRuntimes = new HashMap<String, DownloadRuntime>();
		if( runtimes == null )
			return downloadRuntimes;
		for (DownloadRuntime runtime:runtimes) {
			downloadRuntimes.put(runtime.getId(), runtime);
		}
		return downloadRuntimes;
	}
	
	/* 
	 * This will acquire a list of DownloadRuntimes in a safe way (including progress monitor dialog and jobs that respond to cancel) 
	 */
	private static HashMap<String, DownloadRuntime> dlRuntimeMapFromFilter(IDownloadRuntimeFilter filter) {
		return dlRuntimeMapFromFilter(filter, safeFetchDownloadRuntimes(null));
	}
	
	/**
	 * Given a map of download runtimes to traverse, and a filter, return a map of matching dl runtimes
	 * @param filter
	 * @param downloadRuntimes
	 * @return
	 */
	private static HashMap<String, DownloadRuntime> dlRuntimeMapFromFilter(IDownloadRuntimeFilter filter, Map<String, DownloadRuntime> downloadRuntimes) {
		HashMap<String, DownloadRuntime> tmp = new HashMap<String, DownloadRuntime>();
		Iterator<String> i = downloadRuntimes.keySet().iterator();
		while(i.hasNext()) {
			String k = i.next();
			DownloadRuntime v = downloadRuntimes.get(k);
			if( filter.accepts(v)) {
				tmp.put(k, v);
			}
		}
		return tmp;
	}
	
	/**
	 * A constructor to create the standard pages
	 * 
	 * @param ids     A list of keys the client may wish to put into TaskModel
	 * @param values  A list of values the client may wish to put into TaskModel
	 */
	public DownloadRuntimesTaskWizard(final String[] ids, final String[] values) {
		this(ids, values, null);
	}
	
	/**
	 * A constructor to create the standard page. 
	 * 
	 * @param ids     A list of keys the client may wish to put into TaskModel
	 * @param values  A list of values the client may wish to put into TaskModel
	 * @param map     A map of the download runtimes to display
	 */

	public DownloadRuntimesTaskWizard(final String[] ids, final String[] values, final Map<String, DownloadRuntime> downloadRuntimes) {

		super(Messages.DownloadRuntimesWizard_Download_Runtimes, new WizardFragment(){
			protected void createChildFragments(List<WizardFragment> list) {
				list.add(new SelectDownloadRuntimeFragment(safeFetchDownloadRuntimes(downloadRuntimes)));
				list.add(new DownloadRuntimeLicenseFragment());
				list.add(new FinalizeRuntimeDownloadFragment());
			}
		}, new TaskModel());
		
		if (ids != null) {
			TaskModel taskModel2 = getTaskModel();
			int size = ids.length;
			for (int i = 0; i < size; i++) {
				taskModel2.putObject(ids[i], values[i]);
			}
		}
		
		getTaskModel().putObject(DL_RUNTIME_MAP, safeFetchDownloadRuntimes(downloadRuntimes));
	}

	/*
	 * This method will return the provided downloadRuntimes object if it is not null.
	 * If it is null, it will use a ProgressMonitorDialog and job to fetch an accurate DownloadRuntime map
	 * to ensure there is no blocking if we cannot communicate with the server
	 */
	private static Map<String, DownloadRuntime> safeFetchDownloadRuntimes(Map<String, DownloadRuntime> downloadRuntimes) {
		if( downloadRuntimes != null )
			return downloadRuntimes;
		// Must use a progress monitor dialog to load the download runtimes
		// otherwise ui can freeze for extended duration
		final List<DownloadRuntime> runtimes = new ArrayList<DownloadRuntime>();
		IRunnableWithProgress loadDownloadRuntimes = new IRunnableWithProgress() {
			public void run(IProgressMonitor monitor) throws InvocationTargetException,
					InterruptedException {
				DownloadRuntime[] inner = null;
				inner = RuntimeCoreActivator.getDefault().getDownloadRuntimeArray(monitor);
				if( inner != null ) {
					runtimes.addAll(Arrays.asList(inner));
				}
			}
		};
		
		try {
			new ProgressMonitorDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()).run(true, true, loadDownloadRuntimes);
		} catch (InvocationTargetException e1) {
			RuntimeCoreActivator.pluginLog().logError(e1);
		} catch (InterruptedException e1) {
			RuntimeCoreActivator.pluginLog().logError(e1);
		}

		return dlRuntimeMapFromList(runtimes);
	}
	
	
	public void init(IWorkbench newWorkbench, IStructuredSelection newSelection) {
		// do nothing
	}
}
