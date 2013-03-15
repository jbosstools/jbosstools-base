/******************************************************************************* 
 * Copyright (c) 2007-2013 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *     Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.test.util;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

/**
 * @author eskimo
 * 
 */
public class JobUtils {

	private static final long MAX_IDLE = 20 * 60 * 1000L;
	private static final long DEFAULT_DELAY = 500;

	public static void waitForIdle() {
		waitForIdle(DEFAULT_DELAY);
	}

	public static void waitForIdle(long delay) {
		waitForIdle(delay, MAX_IDLE);
	}

	public static void waitForIdle(long delay, long maxIdle) {
		long start = System.currentTimeMillis();
		while (!Job.getJobManager().isIdle()) {
			delay(delay);
			if ((System.currentTimeMillis() - start) > maxIdle) {
				Job[] jobs = Job.getJobManager().find(null);
				StringBuffer str = new StringBuffer();
				for (Job job : jobs) {
					if (job.getThread() != null) {
						str.append("\n").append(job.getName()).append(" (")
								.append(job.getClass()).append(")");
					}
				}
				throw new RuntimeException(
						"Long running tasks detected:" + str.toString()); //$NON-NLS-1$
			}
		}
	}

	public static void delay(long waitTimeMillis) {
		Display display = Display.getCurrent();
		if(PlatformUI.isWorkbenchRunning() && display!= null) {
			long endTimeMillis = System.currentTimeMillis() + waitTimeMillis;
			while (System.currentTimeMillis() < endTimeMillis) {
				if (!display.readAndDispatch())
					display.sleep();
			}
			display.update();
		} else { // Otherwise, perform a simple sleep.
			try {
				Thread.sleep(waitTimeMillis);
			} catch (InterruptedException e) {
				// Ignored.
			}
		}
	}

	public static void runDeferredEvents() {
		while( Display.getCurrent().readAndDispatch());
	}
}
