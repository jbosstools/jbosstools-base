/*******************************************************************************
 * Copyright (c) 2000, 2013 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     JBoss by Red Hat
 *******************************************************************************/
package org.jboss.tools.runtime.ui.internal.wizard;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.TrayDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.jboss.tools.runtime.core.RuntimeCoreActivator;
import org.jboss.tools.runtime.core.model.DownloadRuntime;
import org.jboss.tools.runtime.core.model.IDownloadRuntimeFilter;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

/**
 * 
 * @author snjeza
 *
 */
public class DownloadRuntimesWizard extends Wizard implements INewWizard {

	private Map<String, DownloadRuntime> downloadRuntimes;
	private boolean helpAvailable;
	private DownloadRuntimesWizardFirstPage firstPage;
	private DownloadRuntimesSecondPage secondPage;
	private DownloadRuntimeLicensePage licensePage;
	private Shell shell;

	public DownloadRuntimesWizard() {
		super();
		setNeedsProgressMonitor(true);
		initializeDefaultPageImageDescriptor();
		setWindowTitle("Download Runtimes");
		saveHelpAvailable();
		downloadRuntimes = RuntimeCoreActivator.getDefault().getDownloadRuntimes();
	}
	
	public DownloadRuntimesWizard(Shell shell) {
		this();
		this.shell = shell;
	}

	public DownloadRuntimesWizard(Shell shell, IDownloadRuntimeFilter filter) {
		this(shell);
		if (filter != null) {
			Map<String, DownloadRuntime> allDownloads = downloadRuntimes;
			Map<String, DownloadRuntime> filtered = new HashMap<String, DownloadRuntime>();	
			Iterator<String> it = allDownloads.keySet().iterator();
			while(it.hasNext()) {
				String k = it.next();
				DownloadRuntime rt = allDownloads.get(k);
				if( filter.accepts(rt)) {
					filtered.put(k, rt);
				}
			}
			downloadRuntimes = filtered;
		}
	}
	
	public DownloadRuntimesWizard(Shell shell, List<DownloadRuntime> runtimes) {
		this(shell);
		Assert.isNotNull(runtimes);
		downloadRuntimes = new HashMap<String, DownloadRuntime>();
		for (DownloadRuntime runtime:runtimes) {
			downloadRuntimes.put(runtime.getId(), runtime);
		}
		
	}
	
	private void saveHelpAvailable() {
		helpAvailable = TrayDialog.isDialogHelpAvailable();
		TrayDialog.setDialogHelpAvailable(false);
	}
	
	private void initializeDefaultPageImageDescriptor() {
		setDefaultPageImageDescriptor(RuntimeUIActivator.imageDescriptorFromPlugin(RuntimeUIActivator.PLUGIN_ID, "icons/DownloadRuntimeWizBan.png"));
	}

	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		
	}

	@Override
	public void addPages() {
		
		if (downloadRuntimes.size() == 1) {
			DownloadRuntime downloadRuntime = downloadRuntimes.values().iterator().next();
			if (downloadRuntime.getLicenceURL() != null) {
				licensePage = new DownloadRuntimeLicensePage(downloadRuntime);
				if (!licensePage.isAccepted(downloadRuntime)) {
					addPage(licensePage);
				}
			}
			secondPage = new DownloadRuntimesSecondPage(downloadRuntime, shell);
			addPage(secondPage);
		} else {
			licensePage = new DownloadRuntimeLicensePage(null);
			secondPage = new DownloadRuntimesSecondPage(null, shell);
			firstPage = new DownloadRuntimesWizardFirstPage(downloadRuntimes, secondPage, licensePage);
			addPage(firstPage);
			addPage(licensePage);
			addPage(secondPage);
		} 
	}

	@Override
	public boolean performFinish() {
		final boolean[] ret = new boolean[1];
		ret[0] = false;
		try {
			getContainer().run(false, true, new IRunnableWithProgress() {

				@Override
				public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
					licensePage.finishPage();
					ret[0] = secondPage.finishPage(monitor);
				}
			});
		} catch (InvocationTargetException e) {
			RuntimeUIActivator.log(e);
			secondPage.setErrorMessage(e.getLocalizedMessage());
		} catch (InterruptedException e) {
			RuntimeUIActivator.log(e);
			secondPage.setErrorMessage(e.getLocalizedMessage());
		}
		if (!ret[0]) {
			getContainer().showPage(secondPage);
		}
		return ret[0];
	}

	@Override
	public void dispose() {
		TrayDialog.setDialogHelpAvailable(helpAvailable);
		super.dispose();
	}

	public DownloadRuntimesWizardFirstPage getFirstPage() {
		return firstPage;
	}
}
