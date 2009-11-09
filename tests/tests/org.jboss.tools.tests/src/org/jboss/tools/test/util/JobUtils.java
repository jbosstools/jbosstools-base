/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 

package org.jboss.tools.test.util;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;

/**
 * @author eskimo
 *
 */
public class JobUtils {
	
	private static final long MAX_IDLE = 20*60*1000L;
	private static final long DEFAULT_DELAY = 500;

	public static void waitForIdle() {
		waitForIdle(DEFAULT_DELAY);
	}
	
	public static void waitForIdle(long delay) {
		long start = System.currentTimeMillis();
		// Job.getJobManager().isIdle() is more efficient than EditorTestHelper.allJobsQuiet()
		// EditorTestHelper.allJobsQuiet() isn't thread-safe
		// https://bugs.eclipse.org/bugs/show_bug.cgi?id=198241 is fixed 
		//while (!EditorTestHelper.allJobsQuiet()) {
		while (!Job.getJobManager().isIdle()) {
			delay(delay);
			if ( (System.currentTimeMillis()-start) > MAX_IDLE ) 
				throw new RuntimeException("A long running task detected"); //$NON-NLS-1$
		}
	}
	
	public static void delay(long waitTimeMillis) {
		Display display = Display.getCurrent();
		if (display != null) {
			long endTimeMillis = System.currentTimeMillis() + waitTimeMillis;
			while (System.currentTimeMillis() < endTimeMillis) {
				if (!display.readAndDispatch())
					display.sleep();
			}
			display.update();
		}
		// Otherwise, perform a simple sleep.
		else {
			try {
				Thread.sleep(waitTimeMillis);
			} catch (InterruptedException e) {
				// Ignored.
			}
		}
	}
}
