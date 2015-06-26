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

import org.eclipse.core.internal.jobs.Semaphore;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.jboss.tools.foundation.core.internal.Trace;

/**
 * This is a job which provides a interruptableJoin() method to make it interruptable. 
 * Use of this class ensures that if you are joined to a job such as this, 
 * and your thread is blocking or waiting on a lock, the job CAN be interrupted. 
 * 
 * Superclass implementation does not propagate {@link InterruptedException} 
 * despite the fact that its javadoc says it does.
 * 
 * This class should NOT be subclassed to do ANY ui work AT ALL, 
 * as the implementation is NOT UI-safe. The safety for 
 * join() and interrupt() may cause a deadlock if the job is 
 * performing any UI functionality at all. Calls to 
 * Display.syncExec() are specifically forbidden. 
 * 
 */
public class InterruptableJoinJob extends Job {
	public InterruptableJoinJob(String name) {
		super(name);
	}

	protected IStatus run(IProgressMonitor monitor) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * A custom implementation of join because the official one
	 * cannot be interrupted at all.   [293312]
	 * 
	 * @throws InterruptedException
	 */
	public void interruptableJoin() throws InterruptedException {
		interruptableJoin(false);
	}
	
	/**
	 * A custom implementation of join because the official one
	 * cannot be interrupted at all.   [293312]
	 * 
	 * If schedule is true, this implementation will be sure to add the listener BEFORE schedule, 
	 * to prevent any issues for fast-completion jobs.
	 * If schedule is false, the user must schedule the job themselves.
	 * 
	 * @throws InterruptedException
	 */
	public void interruptableJoin(boolean schedule) throws InterruptedException {
		Trace.trace(Trace.STRING_FINER, "Joining job " + getName() + " in interruptable fashion");
		final IJobChangeListener listener;
		final Semaphore barrier2;
		barrier2 = new Semaphore(null);
		listener = new JobChangeAdapter() {
			public void done(IJobChangeEvent event) {
				Trace.trace(Trace.STRING_FINER, "Job " + event.getJob().getName() + " completed. Releasing barrier.");
				barrier2.release();
			}
		};
		addJobChangeListener(listener);
		if( schedule ) {
			Trace.trace(Trace.STRING_FINER, "Scheduling Job " + getName());
			schedule();
		}
		try {
			if (barrier2.acquire(Long.MAX_VALUE))
				return;
		} catch (InterruptedException e) {
			Trace.trace(Trace.STRING_FINER, "Job " + getName() + " has been interrupted, so this join is terminating");
			// Actual join implementation LOOPS here, and ignores the exception.
			throw new InterruptedException();
		}
	}
	
	
}
