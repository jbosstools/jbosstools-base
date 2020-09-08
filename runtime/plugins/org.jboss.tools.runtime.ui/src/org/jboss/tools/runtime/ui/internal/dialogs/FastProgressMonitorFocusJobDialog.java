/*******************************************************************************
 * Copyright (c) 2004, 2015 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * IBM - Initial API and implementation
 * Markus Schorn (Wind River Systems)
 * Patrik Suzzi <psuzzi@gmail.com> - Bug 460683
 *******************************************************************************/
package org.jboss.tools.runtime.ui.internal.dialogs;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.internal.progress.ProgressManager;
import org.eclipse.ui.internal.progress.ProgressManager.JobMonitor;
import org.eclipse.ui.internal.progress.ProgressMonitorFocusJobDialog;
import org.jboss.tools.runtime.ui.RuntimeUIActivator;

/**
 * The ProgressMonitorFocusJobDialog is a dialog that shows progress for a
 * particular job in a modal dialog so as to give a user accustomed to a modal
 * UI a more familiar feel.
 */
public class FastProgressMonitorFocusJobDialog extends ProgressMonitorFocusJobDialog {
	
	/**
	 * Create a new instance of the receiver with progress reported on the job.
	 *
	 * @param parentShell
	 *            The shell this is parented from.
	 */
	public FastProgressMonitorFocusJobDialog(Shell parentShell) {
		super(parentShell);
	}
	
	
	
	public void run(boolean fork, boolean cancelable,
            Job j) throws InvocationTargetException,
            InterruptedException {
		Field jeorb;
		IRunnableWithProgress runnable = runnableWithProgress(j);
		Exception ex = null;
		try {
			jeorb = ProgressMonitorFocusJobDialog.class.getDeclaredField("job");
			jeorb.setAccessible(true);
			jeorb.set(this, j);
			super.run(fork, cancelable, runnable);
			return;
		} catch (NoSuchFieldException e) {
			ex = e;
		} catch (SecurityException e) {
			ex = e;
		} catch (IllegalArgumentException e) {
			ex = e;
		} catch (IllegalAccessException e) {
			ex = e;
		} 
		if( ex != null ) {
			RuntimeUIActivator.pluginLog().logWarning("Unable to create FastProgressMonitorFocusJobDialog", ex);
		}
		// Something went wrong with the reflection probably... 
		// let's open a real dialog for them, even if its slow
		new ProgressMonitorFocusJobDialog(getParentShell()).run(fork, cancelable, runnable);
	}

	private IRunnableWithProgress runnableWithProgress(final Job longJob) {
		IRunnableWithProgress runnable = new IRunnableWithProgress(){
			public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
				addRunableMonitorAsListener(longJob, monitor);
				longJob.join();
			}
		};
		return runnable;
	}
	
	private void addRunableMonitorAsListener(Job longJob, IProgressMonitor listener) {
		JobMonitor jobMonitor = ProgressManager.getInstance().progressFor(longJob);
		try {
			jobMonitor.addProgressListener(listener);
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
