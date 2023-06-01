/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.core.jobs;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;

/**
 * A Future that allows you to wait with a timeout for the result of a job.
 * 
 * @author André Dietisheim
 */
public class JobResultFuture implements Future<IStatus> {

	private AtomicBoolean done = new AtomicBoolean(false);
	private AtomicBoolean cancelled = new AtomicBoolean(false);
	private ArrayBlockingQueue<IStatus> queue = new ArrayBlockingQueue<IStatus>(1);
	private Job job;

	public JobResultFuture(Job job) {
		this.job = job;
		addJobFinishedListener(job);
	}

	@Override
	public boolean cancel(boolean mayInterruptIfRunning) {
		if (!isRunning(job) || mayInterruptIfRunning) {
			cancelled.set(true);
			job.cancel();
			Thread thread = job.getThread();
			if(thread != null) {
				thread.interrupt();
			}
		}
		return isRunning(job);
	}

	private boolean isRunning(Job job) {
		return job.getState() == Job.RUNNING;
	}

	@Override
	public boolean isCancelled() {
		return cancelled.get();
	}

	@Override
	public boolean isDone() {
		if (isCancelled()) {
			return false;
		}
		return done.get();
	}

	@Override
	public IStatus get() throws InterruptedException, ExecutionException {
		return queue.poll();
	}

	@Override
	public IStatus get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException,
			TimeoutException {
		return queue.poll(timeout, unit);
	}

	private void addJobFinishedListener(final Job job) {
		job.addJobChangeListener(new JobChangeAdapter() {

			@Override
			public void done(IJobChangeEvent event) {
				queue.offer(event.getResult());
				done.set(true);
			}
		});
	}
}
