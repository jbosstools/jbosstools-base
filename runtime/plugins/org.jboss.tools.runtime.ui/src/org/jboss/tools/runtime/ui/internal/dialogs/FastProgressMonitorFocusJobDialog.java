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
import java.lang.reflect.Method;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IProgressMonitorWithBlocking;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.IPreferenceConstants;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.eclipse.ui.internal.WorkbenchPlugin;
import org.eclipse.ui.internal.progress.ProgressManager;
import org.eclipse.ui.internal.progress.ProgressMonitorFocusJobDialog;
import org.eclipse.ui.progress.IProgressConstants;
import org.eclipse.ui.progress.WorkbenchJob;
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
				addRunableMonitorAsListener(longJob, getBlockingProgressMonitor(monitor));
				longJob.join();
			}
		};
		return runnable;
	}
	
	private IProgressMonitorWithBlocking getBlockingProgressMonitor(final IProgressMonitor monitor) {
		IProgressMonitorWithBlocking listener = new IProgressMonitorWithBlocking(){
			public void beginTask(String name, int totalWork) {
				monitor.beginTask(name, totalWork);
			}
			public void done() {
				monitor.done();
			}
			public void internalWorked(double work) {
				monitor.internalWorked(work);
			}
			public boolean isCanceled() {
				return monitor.isCanceled();
			}
			public void setCanceled(boolean value) {
				monitor.setCanceled(value);
			}
			public void setTaskName(String name) {
				monitor.setTaskName(name);
			}
			public void subTask(String name) {
				monitor.subTask(name);
			}
			public void worked(int work) {
				monitor.worked(work);
			}
			public void setBlocked(IStatus reason) {
				if( monitor instanceof IProgressMonitorWithBlocking) {
					((IProgressMonitorWithBlocking)monitor).setBlocked(reason);
				}
			}
			public void clearBlocked() {
				if( monitor instanceof IProgressMonitorWithBlocking) {
					((IProgressMonitorWithBlocking)monitor).clearBlocked();
				}
			}
		};
		return listener;
	}
	
	private void addRunableMonitorAsListener(Job longJob, IProgressMonitorWithBlocking listener) {
		IProgressMonitorWithBlocking jobMonitor = ProgressManager.getInstance().progressFor(longJob);
		try {
			Class jMon =  Class.forName("org.eclipse.ui.internal.progress.ProgressManager$JobMonitor");
			Class[] cArg = new Class[]{IProgressMonitorWithBlocking.class};
			Method m = jMon.getDeclaredMethod("addProgressListener", cArg);
			m.setAccessible(true);
			m.invoke(jobMonitor, listener);
			System.out.println(jMon);
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
