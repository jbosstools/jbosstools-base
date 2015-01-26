/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui;

import java.lang.reflect.InvocationTargetException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.foundation.core.jobs.DelegatingProgressMonitor;

/**
 * @author André Dietisheim
 */
public class WizardUtils {

	private static final long THREAD_SLEEP = 1 * 1000;
	private static final int NO_TIMEOUT = -1;

	private WizardUtils() {
		// inhibit instantiation
	}

	/**
	 * Runs the given job in the given wizard container. This method will return
	 * immediately, it will not wait for the job completion.
	 * <p>
	 * In order to have the wizard displaying a progress bar, you need to set
	 * Wizard#setNeedsProgressMonitor to <code>true</code>.
	 * 
	 * @param job
	 *            the job to run
	 * @param container
	 *            the wizard container to run the job in
	 * @return a future that allows you to wait for the job result.
	 * @throws InvocationTargetException
	 *             the invocation target exception
	 * @throws InterruptedException
	 *             the interrupted exception
	 * @author André Dietisheim
	 * @see IWizardContainer#run(boolean, boolean, IRunnableWithProgress)
	 * @see Job
	 */
	public static IStatus runInWizard(final Job job, IWizardContainer container) throws InvocationTargetException,
			InterruptedException {
		return runInWizard(job, null, container);
	}

	/**
	 * Runs the given job in the given wizard container.
	 * <p>
	 * In order to have the wizard displaying a progress bar, you need to set
	 * Wizard#setNeedsProgressMonitor to <code>true</code>.
	 * 
	 * @param job
	 *            the job to run in the wizard
	 * @param delegatingMonitor
	 *            the delegating monitor that the wizard monitor shall be added
	 *            to.
	 * @param container
	 *            the wizard container to run the job in
	 * @return a future that allows you to wait for the job result
	 * @throws InvocationTargetException
	 * @throws InterruptedException
	 */
	public static IStatus runInWizard(final Job job, final DelegatingProgressMonitor delegatingMonitor,
			final IWizardContainer container) throws InvocationTargetException, InterruptedException {
		return runInWizard(job, delegatingMonitor, container, NO_TIMEOUT);
	}

	/**
	 * Runs the given job in the given wizard container.
	 * <p>
	 * In order to have the wizard displaying a progress bar, you need to set
	 * Wizard#setNeedsProgressMonitor to <code>true</code>.
	 * <p>
	 * In order to be able to report updates in the job to the wizard progress,
	 * you'd have to use the {@link DelegatingProgressMonitor}. Add your job
	 * monitor to that aggregating monitor and call #subTask #done etc. on that
	 * one (@link https://bugs.eclipse.org/bugs/show_bug.cgi?id=293098)
	 * 
	 * <code>
	 * DelegatingProgressMonitor delegate = new DelegatingProgressMonitor();
	 * delegate.add(myJobMonitor)
	 * delegate.add(wizardMonitor)
	 * ...
	 * delegate.subTask("now reporting to the delegate so that progress is shown in workbench and wizard");
	 * </code>
	 * 
	 * @param job
	 *            the job to run in the wizard
	 * @param delegatingMonitor
	 *            the delegating monitor that the wizard monitor shall be added
	 *            to.
	 * @param container
	 *            the wizard container to run the job in
	 * @return a future that allows you to wait for the job result
	 * @throws InvocationTargetException
	 * @throws InterruptedException
	 */
	public static IStatus runInWizard(final Job job, final DelegatingProgressMonitor delegatingMonitor,
			IWizardContainer container, final long timeout) throws InvocationTargetException, InterruptedException {
		final JobResultFuture future = new JobResultFuture(job);
		container.run(true, true, new IRunnableWithProgress() {
			@Override
			public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
				if (delegatingMonitor != null) {
					delegatingMonitor.add(monitor);
				}

				monitor.beginTask(job.getName(), IProgressMonitor.UNKNOWN);
				job.schedule();
				try {
					waitForFuture(timeout, future, monitor);
				} catch (ExecutionException e) {
				} catch (TimeoutException e) {
				} finally {
					monitor.done();
				}
			}
		});

		return getStatus(job, future);
	}

	private static void waitForFuture(long timeout, JobResultFuture future, IProgressMonitor monitor)
			throws InterruptedException, ExecutionException, TimeoutException {
		long startTime = System.currentTimeMillis();
		while (!future.isDone()) {
			if ((timeout > 0 && isTimeouted(startTime, timeout))
					|| monitor.isCanceled()) {
				future.cancel(true);
				break;
			}
			
			Thread.sleep(THREAD_SLEEP);
		}
	}

	private static boolean isTimeouted(long startTime, long timeout) {
		return (System.currentTimeMillis() - startTime) > timeout;
	}
	
	private static IStatus getStatus(final Job job, final JobResultFuture future) {

		if (future.isCancelled()) {
			String message = NLS.bind("The operation ''{0}'' was cancelled", job.getName());
			CommonUIPlugin.getDefault().logError(message);
			return new Status(IStatus.CANCEL, CommonUIPlugin.PLUGIN_ID, message);
		}
		if (future.isDone()) {
			return job.getResult();
		}
		String message =
				NLS.bind("The operation ''{0}'' did not complete in a reasonnable amount of time", job.getName());
		CommonUIPlugin.getDefault().logError(message);
		return new Status(IStatus.ERROR, CommonUIPlugin.PLUGIN_ID, message);
	}

	/**
	 * Runs the given job in the given wizard container.
	 * <p>
	 * Furhtermore it updates the models and targets of the given data binding
	 * context. This might be necessary if the given job will change widget
	 * enablements in the calling wizard page. The reason for is that the runner
	 * saves the widget enablement states when it's up to execute the runnable.
	 * It then restores those states once he finished executing the runnable. It
	 * may therefore restore incorrect states since the job changed the
	 * enablements when it was run.
	 * 
	 * @param job
	 *            the job
	 * @param container
	 *            the container
	 * @param dbc
	 *            the dbc
	 * @throws InvocationTargetException
	 *             the invocation target exception
	 * @throws InterruptedException
	 *             the interrupted exception
	 */
	public static IStatus runInWizard(final Job job, IWizardContainer container, DataBindingContext dbc)
			throws InvocationTargetException, InterruptedException {
		return runInWizard(job, null, container, dbc);
	}

	public static IStatus runInWizard(Job job, DelegatingProgressMonitor monitor, IWizardContainer container,
			DataBindingContext dbc) throws InvocationTargetException, InterruptedException {
		IStatus status = runInWizard(job, monitor, container);
		// re-propagate model states to widgets states after those were disabled by wizard runnable
		dbc.updateTargets();
		// dont update models, they're already in correct state and this would
		// possibly override correct model state with erroneous widget state 
		// dbc.updateModels();
		return status;
	}

	/**
	 * Flips to the next wizard page or finishes the current wizard.
	 * 
	 * @param wizardPage
	 *            the wizard page this call is executed in
	 */
	public static void nextPageOrFinish(IWizardPage wizardPage) {
		IWizard wizard = wizardPage.getWizard();
		if (wizardPage.canFlipToNextPage()) {
			IWizardPage nextPage = wizard.getNextPage(wizardPage);
			wizard.getContainer().showPage(nextPage);
		} else if (wizard.canFinish()) {
			if (wizard.performFinish()) {
				wizard.getContainer().getShell().close();
			}
		}
	}

	public static int openWizardDialog(IWizard wizard, Shell shell) {
		WizardDialog dialog = new WizardDialog(shell, wizard);
		dialog.create();
		return dialog.open();
	}
}
