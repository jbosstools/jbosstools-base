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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * An IProgressMonitor that aggregates other monitors and delegates call to the
 * monitors that were added to it.
 * 
 * @author André Dietisheim
 */
public class DelegatingProgressMonitor implements IProgressMonitor {

	List<IProgressMonitor> monitors = new ArrayList<IProgressMonitor>();

	public synchronized void add(IProgressMonitor monitor) {
		if (monitors.size() > 0) {
			IProgressMonitor removed = monitors.remove(0);
			monitors.add(monitor);
			monitors.add(removed);
		} else {
			monitors.add(monitor);
		}
	}

	@Override
	public synchronized void subTask(final String name) {
		for (IProgressMonitor monitor : monitors) {
			monitor.subTask(name);
		}
	}

	@Override
	public synchronized void beginTask(final String name, final int totalWork) {
		for (IProgressMonitor monitor : monitors) {
			monitor.beginTask(name, totalWork);
		}
	}

	@Override
	public synchronized void done() {
		for (IProgressMonitor monitor : monitors) {
			monitor.done();
		}
	}

	@Override
	public synchronized void internalWorked(final double work) {
		for (IProgressMonitor monitor : monitors) {
			monitor.internalWorked(work);
		}
	}

	@Override
	public synchronized boolean isCanceled() {
		for (IProgressMonitor monitor : monitors) {
			if (monitor.isCanceled()) {
				return true;
			}
		}
		return false;
	}

	@Override
	public synchronized void setCanceled(final boolean value) {
		for (IProgressMonitor monitor : monitors) {
			monitor.setCanceled(value);
		}
	}

	@Override
	public synchronized void setTaskName(final String name) {
		for (IProgressMonitor monitor : monitors) {
			monitor.setTaskName(name);
		}
	}

	@Override
	public synchronized void worked(final int work) {
		for (IProgressMonitor monitor : monitors) {
			monitor.worked(work);
		}
	}
}
