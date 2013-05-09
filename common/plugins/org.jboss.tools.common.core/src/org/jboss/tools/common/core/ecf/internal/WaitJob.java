/*******************************************************************************
 * Copyright (c) 2013 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.core.ecf.internal;

import org.eclipse.core.internal.jobs.Semaphore;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;

public class WaitJob extends Job {
	/**
	 * Waits until the first entry in the given array is non-null.
	 * Launch this job synchronously. 
	 * 
	 * @param jobName
	 * @param barrier a barrier to wait on
	 * @param system whether the launched job is to be a system job or not
	 * @throws InterruptedException if it is interrupted OR if the job is canceled
	 */
	public static void waitForSynchronous(String jobName, Object[] barrier, 
			boolean system) throws InterruptedException {
		WaitJob wait = new WaitJob(jobName, barrier, system);
		wait.schedule();
		try {
			// Do not simply join, because then there is *NO* way to interrupt this at ALL
			// [293312]
			// Instead, do a cutom-join
			customJoin(wait);
		} catch (InterruptedException e) {
			// Do NOT log the error. Let the caller log it as they wish.
			// Clean up the job, since I'm the only one who has a reference to it.
			wait.cancel();
			
			// re-throw the interrupted exception, so whoever was waiting
			// knows to clean up if they can.
			throw e;
		}
		if( wait.hasBeenCanceled()) {
			throw new InterruptedException();
		}
	}
	
	/**
	 * A custom implementation of join because the official one
	 * cannot be interrupted at all.   [293312]
	 * @param j
	 * @throws InterruptedException
	 */
	private static void customJoin(Job j) throws InterruptedException {
		final IJobChangeListener listener;
		final Semaphore barrier2;
		barrier2 = new Semaphore(null);
		listener = new JobChangeAdapter() {
			public void done(IJobChangeEvent event) {
				barrier2.release();
			}
		};
		j.addJobChangeListener(listener);
		try {
			if (barrier2.acquire(Long.MAX_VALUE))
				return;
		} catch (InterruptedException e) {
			// Actual join implementation LOOPS here, and ignores the exception.
			throw new InterruptedException();
		}
	}
	
	/**
	 * The barrier to block on
	 */
	private final Object[] barrier;
	
	/**
	 * Whether this job has been canceled
	 */
	private boolean canceled = false;
	
	/**
	 * Creates a wait job. This job will wait specifically
	 * for the first element of the array to be non-null. 
	 * 
	 * 
	 * @param jobName A name for the job
	 * @param barrir The job will wait until the first entry in the barrier is non-null
	 * @param system Is this a system-level job
	 */
	public WaitJob(String jobName, Object[] barrier, boolean system) {
		super(jobName);
		this.barrier = barrier;
		setSystem(system);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
	 */
	protected IStatus run(IProgressMonitor monitor) {
		synchronized (barrier) {
			while (!monitor.isCanceled() && barrier[0] == null) {
				try {
					barrier.wait();
				} catch (InterruptedException e) {
					//ignore
				}
			}
		}
		return Status.OK_STATUS;
	}
	
	/**
	 * Ensure that if the job is canceled, we interrupt the barrier
	 */
	protected void canceling() {
		synchronized( barrier ) {
			canceled = true;
			barrier.notify();
		}
	}
	
	/**
	 * Allow our synchronous static method to verify we've been canceled
	 * @return
	 */
	private boolean hasBeenCanceled() {
		synchronized(barrier) {
			return canceled;
		}
	}
}