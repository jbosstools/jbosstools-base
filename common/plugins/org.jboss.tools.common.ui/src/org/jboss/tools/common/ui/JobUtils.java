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
package org.jboss.tools.common.ui;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;

/**
 * @author André Dietisheim
 *
 */
public class JobUtils {

	private JobUtils() {
		// inhibit instantiation
	}

	public static IStatus waitForJobResult(Job job, long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
		return new JobResultFuture(job).get(timeout, unit);
	}

	public static boolean isOk(IStatus status) {
		return status != null
				&& status.isOK();
	}

	public static boolean isWarning(IStatus status) {
		return status != null
				&& status.getSeverity() == IStatus.WARNING;
	}

	public static boolean isCancel(IStatus status) {
		return status != null
				&& status.getSeverity() == IStatus.CANCEL;
	}

}
