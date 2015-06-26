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
import org.eclipse.core.runtime.jobs.Job;
import org.jboss.tools.foundation.core.internal.Trace;


/**
 * This class is a Job with the purpose of specifically 
 * waiting on a barrier array to have a non-null value in the 
 * index 0 slot,  OR, to have a canceled progress monitor. 
 * 
 * This job does not guarantee an immediate return when an interrupt is received. 
 * The job does its best to clean up itself
 * after an interrupt is received, but it is up to the delegating runnable 
 * to handle such interrupts in an appropriate time.  
 * 
 * The purpose of this job is to ensure that a workflow exists to return as fast as possible
 * when a progress monitor is canceled. This class is most useful for tasks that may block
 * for a duration and may not have an opportunity to check for interrupts or a canceled flag. 
 * This job will segregate those threads into the background and clean them up in time, 
 * but primarily to ensure the fast return time upon cancelation.
 * 
 * Proper use of this job is as follows:
 * 
 *  BarrierProgressWaitJob j = new BarrierProgressWaitJob(name, runnable);
 *  j.schedule();
 *	// This join will also poll the provided monitor for cancelations
 *	j.monitorSafeJoin(monitor);
 *	if( j.getReturnValue() != null)
 *		return (ReturnType)j.getReturnValue();
 *
 * This job guarantees that a call to monitorSafeJoin() will return within 200ms of a 
 * cancelation in the progress monitor, or immediately upon the completion of the 
 * delegate runnable's execution, whichever comes first. 
 * 
 * In the event that a cancelation is registered, and the delegate runnable 
 * completes within the 200 ms, the Job may be recognized as both canceled and 
 * still have a valid return value from the delegate runnable. Clients should
 * decide whether they wish to prioritize their logic for a canceled call, 
 * or use the return value. 
 */
public class BarrierProgressWaitJob extends Job {

	/** 
	 * An interface for use with this job type. 
	 */
	public interface IRunnableWithProgress {
		/**
		 * Run the given runnable and hold the return value for inspection later.
		 * @param monitor
		 * @return
		 * @throws Exception
		 */
	    public Object run(IProgressMonitor monitor) throws Exception;
	}
	
	
	private IRunnableWithProgress runnable;
	private Object[] barrier;
	private boolean throwableCaught = false;
	
	public BarrierProgressWaitJob(String name, IRunnableWithProgress runnable) {
		super(name);
		setSystem(true);
		this.runnable = runnable;
		barrier = new Object[1];
	}
	
	protected IStatus run(IProgressMonitor monitor) {
		Trace.trace(Trace.STRING_FINER, "Launching job " + getName() + ", a BarrierProgressWaitJob");
		try {
			Object ret = runnable.run(monitor);
			synchronized(barrier) {
				barrier[0] = ret;
				barrier.notify();
				return Status.OK_STATUS;
			}
		}catch(Exception e) {
			Trace.trace(Trace.STRING_FINER, "Job " + getName() + ", a BarrierProgressWaitJob, failed with exception " + e.getMessage());
			this.throwableCaught = true;
			synchronized(barrier) {
				barrier.notify();
				barrier[0] = e;
				return Status.OK_STATUS;
			}
		}
	}
	
	/**
	 * While the Job is running and doing the work, ensure that the progress monitor
	 * provided as a parameter is respected when canceled. 
	 * 
	 * This method effectively waits for either the Job to complete, or 
	 * for the provided progress monitor to be canceled. In the event of the latter, 
	 * 
	 * This method guarantees a return time equal to that of the delegate method, 
	 * or, a return within 200 ms of the progress monitor being canceled
	 * 
	 * @param monitor
	 */
	public void monitorSafeJoin(IProgressMonitor monitor) {
		// Since we're waiting for the barrier OR the monitor,
		// we must wait only for small intervals, then check the monitor, then loop
		// This ensures that if the monitor is canceled, we respond within 200 ms. 
		boolean done = false;
		while( !done ) {
			synchronized(barrier) {
				try {
					barrier.wait(200);
				} catch (InterruptedException e) {
				}
				if( barrier[0] != null || monitor.isCanceled()) {
					Trace.trace(Trace.STRING_FINER, "job " + getName() + ", a BarrierProgressWaitJob, is finished due to " + 
							(barrier[0] != null ? "a non-null result" : "a canceled progress monitor"));
					done = true;
				}
			}
		}
		// If this monitor is canceled, cancel the job immediately
		if( monitor.isCanceled()) {
			Trace.trace(Trace.STRING_FINER, "Progress monitor for job " + getName() + ", a BarrierProgressWaitJob, has been canceled");
			cancel();
		}
	}
	
	/**
	 * This job has been set to canceled. Interrupt the thread doing the work. 
	 */
	protected void canceling() {
		getThread().interrupt();
	}
	
	/**
	 * Get any throwable which may have been caught during execution.
	 * @return
	 */
	public Throwable getThrowable() {
		if( throwableCaught && barrier[0] instanceof Throwable )
			return (Exception)barrier[0];
		return null;
	}
	
	
	/**
	 * Get the return value of the delegate runnable
	 * @return
	 */
	public Object getReturnValue() {
		return throwableCaught ? null : barrier[0];
	}
}