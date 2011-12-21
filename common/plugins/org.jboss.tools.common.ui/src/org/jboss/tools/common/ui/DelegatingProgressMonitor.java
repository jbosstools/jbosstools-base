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

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * An IProgressMonitor that aggregates other monitors and delegates call to the monitors
 * that were added to it.
 * 
 * @author André Dietisheim
 */
public class DelegatingProgressMonitor implements IProgressMonitor {

	List<IProgressMonitor> monitors = new CopyOnWriteArrayList<IProgressMonitor>();

	public void add(IProgressMonitor monitor) {
		monitors.add(monitor);
	}

	@Override
	public void subTask(final String name) {
		doForAllMonitors(new IMonitorOperation() {

			@Override
			public void doWithMonitor(IProgressMonitor monitor) {
				monitor.subTask(name);
			}
		});
	}

	@Override
	public void beginTask(final String name, final int totalWork) {
		doForAllMonitors(new IMonitorOperation() {

			@Override
			public void doWithMonitor(IProgressMonitor monitor) {
				monitor.beginTask(name, totalWork);
			}
		});
	}

	@Override
	public void done() {
		doForAllMonitors(new IMonitorOperation() {

			@Override
			public void doWithMonitor(IProgressMonitor monitor) {
				monitor.done();
			}
		});
	}

	@Override
	public void internalWorked(final double work) {
		doForAllMonitors(new IMonitorOperation() {

			@Override
			public void doWithMonitor(IProgressMonitor monitor) {
				monitor.internalWorked(work);
			}
		});

	}

	@Override
	public boolean isCanceled() {
		for (IProgressMonitor monitor : monitors) {
			if (monitor.isCanceled()) {
				return true;
			}
		}
		return false;
	}

	@Override
	public void setCanceled(final boolean value) {
		doForAllMonitors(new IMonitorOperation() {

			@Override
			public void doWithMonitor(IProgressMonitor monitor) {
				monitor.setCanceled(value);
			}
		});
	}

	@Override
	public void setTaskName(final String name) {
		doForAllMonitors(new IMonitorOperation() {

			@Override
			public void doWithMonitor(IProgressMonitor monitor) {
				monitor.setTaskName(name);
			}
		});

	}

	@Override
	public void worked(final int work) {
		doForAllMonitors(new IMonitorOperation() {

			@Override
			public void doWithMonitor(IProgressMonitor monitor) {
				monitor.worked(work);
			}
		});
	}

	private void doForAllMonitors(IMonitorOperation operation) {
		for (IProgressMonitor monitor : monitors) {
			operation.doWithMonitor(monitor);
		}
	}

	private interface IMonitorOperation {
		public void doWithMonitor(IProgressMonitor monitor);
	}
}
