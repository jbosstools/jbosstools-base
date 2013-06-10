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
package org.jboss.tools.foundation.core.jobs;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * This class is a Job with the purpose of specifically 
 * waiting on a barrier array to have a non-null value in the 
 * index 0 slot.  It includes implementations that ensure 
 * safety for join() and interrupt() calls. 
 * 
 * This class should NOT be subclassed to do ANY ui work AT ALL, 
 * as the implementation is NOT UI-safe. The safety for 
 * join() and interrupt() may cause a deadlock if the job is 
 * performing any UI functionality at all. Calls to 
 * Display.syncExec() are specifically forbidden. 
 */
public class BarrierWaitJob extends InterruptableJoinJob {
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
		BarrierWaitJob wait = new BarrierWaitJob(jobName, barrier, system);
		try {
			// Do not simply join, because then there is *NO* way to interrupt this at ALL
			// [293312] Instead, do a cutom-join
			wait.interruptableJoin(true);
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
	public BarrierWaitJob(String jobName, Object[] barrier, boolean system) {
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