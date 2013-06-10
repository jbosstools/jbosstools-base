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
package org.jboss.tools.foundation.core.test.jobs;

import junit.framework.TestCase;

import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.jboss.tools.foundation.core.jobs.BarrierWaitJob;
import org.junit.Test;

public class WaitJobTest extends TestCase {
	@Test
	public void testWaitJob() {
		Object[] barrier = new Object[1];
		barrier[0] = null;
		
		final Boolean[] done = new Boolean[1];
		done[0] = null;
		
		BarrierWaitJob wj = new BarrierWaitJob("SomeName", barrier, true);
		wj.addJobChangeListener(new JobChangeAdapter() {
			public void done(IJobChangeEvent arg0) {
				done[0] = true;
			}
		});
		wj.schedule();
		try {
			Thread.sleep(300);
		} catch(InterruptedException ie) {
		}
		// Assert the thread started, and is now asleep
		assertEquals(done[0], null);
		synchronized(barrier) {
			barrier.notify();
		}
		try {
			Thread.sleep(200);
		} catch(InterruptedException ie) {
		}
		// Assert the thread awoke, and is now asleep
		assertEquals(done[0], null);
		
		synchronized(barrier) {
			barrier[0] = new Object();
			barrier.notify();
		}
		try {
			Thread.sleep(200);
		} catch(InterruptedException ie) {
		}
		// Assert the thread awoke, and is now asleep
		assertEquals(done[0], new Boolean(true));
	}
	
	@Test
	public void testWaitJobSynchronousCancel() {
		final Object[] barrier = new Object[1];
		barrier[0] = null;
		new Thread() {
			public void run() {
				try {
					Thread.sleep(500);
				} catch(InterruptedException ie) {
				}
				Job j = findJob("Name1");
				j.cancel();
			}
		}.start();
		try {
			BarrierWaitJob.waitForSynchronous("Name1", barrier, true);
		} catch(InterruptedException ie) {
			// This is correct behavior. 
			return;
		}
		fail("Interrupted Exception was expected");
	}
	
//  [293312]  eclipse bug:  Job.join() can not be interrupted even though the javadoc says it can.
//  This may one day be fixed 
	
	@Test
	public void testWaitJobSynchronousInterrupted() {
		final Object[] barrier = new Object[1];
		barrier[0] = null;
		final Thread testThread = Thread.currentThread();
		
		new Thread() {
			public void run() {
				try {
					Thread.sleep(500);
				} catch(InterruptedException ie) {
				}
				synchronized(testThread) {
					testThread.notify();
					testThread.interrupt();
				}
			}
		}.start();
		try {
			BarrierWaitJob.waitForSynchronous("Name5", barrier, true);
		} catch(InterruptedException ie) {
			// This is expected. Let's wait a short bit and ensure our job was canceled
			try {
				Thread.sleep(300);
			} catch(InterruptedException ie2) {
				
			}
			Job[] jobs = Job.getJobManager().find(null);
			for( int i = 0; i < jobs.length; i++ ) {
				if( jobs[i].getName().equals("Name5")) {
					fail("Job should not be running any longer");
				}
			}
			return;
		}
		fail("Interrupted Exception was expected");
	}
	
	private Job findJob(String name) {
		Job[] jobs = Job.getJobManager().find(null);
		for( int i = 0; i < jobs.length; i++ ) {
			if( jobs[i].getName().equals(name)) {
				return jobs[i];
			}
		}
		return null;
	}
}
