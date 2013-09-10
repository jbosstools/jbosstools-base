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

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbench;
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
	 * 
	 * A constructor that accepts a filter to display only a 
	 * subset of the possible DownloadRuntime objects
	 * 
	 * @param filter
	 */
	public DownloadRuntimesTaskWizard(IDownloadRuntimeFilter filter) {
		this();
		HashMap<String, DownloadRuntime> tmp = new HashMap<String, DownloadRuntime>();
		Map<String, DownloadRuntime> downloadRuntimes = RuntimeCoreActivator.getDefault().getDownloadRuntimes(new NullProgressMonitor());
		Iterator<String> i = downloadRuntimes.keySet().iterator();
		while(i.hasNext()) {
			String k = i.next();
			DownloadRuntime v = downloadRuntimes.get(k);
			if( filter.accepts(v)) {
				tmp.put(k, v);
			}
		}
		downloadRuntimes = tmp;
		getTaskModel().putObject(DL_RUNTIME_MAP, downloadRuntimes);
	}
	
	/**
	 * A constructor to present the wizard with a pre-defined list
	 * of DownloadRuntime objects.
	 * 
	 * @param runtimes
	 */
	public DownloadRuntimesTaskWizard(List<DownloadRuntime> runtimes) {
		this();
		HashMap<String, DownloadRuntime> downloadRuntimes = new HashMap<String, DownloadRuntime>();
		for (DownloadRuntime runtime:runtimes) {
			downloadRuntimes.put(runtime.getId(), runtime);
		}
		getTaskModel().putObject(DL_RUNTIME_MAP, downloadRuntimes);
	}
	
	/**
	 * A constructor to create the standard page. 
	 * 
	 * @param ids     A list of keys the client may wish to put into TaskModel
	 * @param values  A list of values the client may wish to put into TaskModel
	 */
	public DownloadRuntimesTaskWizard(final String[] ids, final String[] values) {
		super(Messages.DownloadRuntimesWizard_Download_Runtimes, new WizardFragment(){
			protected void createChildFragments(List<WizardFragment> list) {
				list.add(new SelectDownloadRuntimeFragment());
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
	}

	public void init(IWorkbench newWorkbench, IStructuredSelection newSelection) {
		// do nothing
	}
}
