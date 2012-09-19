/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model;

import java.util.*;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.internal.decorators.DecoratorManager;
import org.eclipse.ui.progress.UIJob;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class XJob extends WorkspaceJob {
	public static Object FAMILY_XJOB = new Object();
	
	private static boolean suspended = false;

	public interface XRunnable extends Runnable {
		public String getId();
	}
	
	static XJob JOB = new XJob();
	
	public static void addRunnable(XRunnable runnable) {
		JOB.addRunnableInternal(runnable);
	}

	public static void addRunnableWithPriority(XRunnable runnable) {
		JOB.addRunnableInternalWithPriority(runnable);
	}

	public static void waitForJob() throws InterruptedException {
		waitForJob(false);
	}

	public static void shutdown() {
		setSuspended(true);
		synchronized (JOB) {
			JOB.ids.clear();
			JOB.list.clear();
		}
		if(JOB.isRunning()) {
			JOB.cancel();
		}
	}

	public static void waitForJob(boolean immediateOnly) throws InterruptedException {
		Object[] o = {
			XJob.FAMILY_XJOB, ResourcesPlugin.FAMILY_AUTO_REFRESH, ResourcesPlugin.FAMILY_AUTO_BUILD
		};
		while(true) {
			boolean stop = true;
			for (int i = 0; i < o.length; i++) {
				Job[] js = Platform.getJobManager().find(o[i]);
				if(js != null && js.length > 0) {
					Platform.getJobManager().join(o[i], new NullProgressMonitor());
					stop = false;
				}
			}
			if(stop && !immediateOnly) {
				Job running = getJobRunning(10);
				if(running != null) {
					running.join();
					stop = false;
				}
			}
			if(stop) break;
		}
	}
	
	public static Job getJobRunning(int iterationLimit) {
		Job[] js = Platform.getJobManager().find(null);
		Job dm = null;
		if(js != null) for (int i = 0; i < js.length; i++) {
			if(js[i].getState() == Job.RUNNING && js[i].getThread() != Thread.currentThread()) {
				if(js[i] instanceof UIJob) continue;
				if(js[i].belongsTo(DecoratorManager.FAMILY_DECORATE)) {
					dm = js[i];
					continue;
				}
				//TODO keep watching 
				return js[i];
			}
		}
		if(dm != null) {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				//ignore
			}
			if(iterationLimit > 0)
				return getJobRunning(iterationLimit - 1);
		}
		return null;
		
	}

	Set<String> ids = new HashSet<String>();
	List<XRunnable> list = new ArrayList<XRunnable>();

	public XJob() {
		super("JBoss Tools Model Update");
		setSystem(true);
	}

	public boolean belongsTo(Object family) {
		return family == FAMILY_XJOB;
	}
	
	void addRunnableInternal(XRunnable runnable) {
		if (!isSuspended()) {
			synchronized (this) {
				if (ids.contains(runnable.getId()))
					return;
				ids.add(runnable.getId());
				list.add(runnable);
			}
			if (getState() == Job.NONE || !isRunning()) {
				schedule(1000);
			}
		}
	}

	private boolean isRunning() {
		synchronized(this) {
			return running;
		}
	}

	void addRunnableInternalWithPriority(XRunnable runnable) {
		if (!isSuspended()) {
			synchronized (this) {
				if (ids.contains(runnable.getId()))
					return;
				ids.add(runnable.getId());
				list.add(0, runnable);
			}
			if (getState() == Job.NONE || !isRunning()) {
				schedule(0);
			}
		}
	}
	
	boolean running = false;

	@Override
	public IStatus runInWorkspace(IProgressMonitor monitor)
			throws CoreException {
		synchronized(this) {
			running = true;
		}
		while(true) {
			XRunnable r = null;
			synchronized (this) {
				if(list.size() == 0) {
					running = false;
					break;
				}
				r = list.remove(0);
			}
			// monitor.subTask(r.getId()) is irrelevant for system jobs 
			//monitor.subTask(r.getId());
			// XJob is a class from the org.jboss.tools.common.model plugin. This plugin must be active
			//int state = 0;
			//Bundle b = Platform.getBundle("org.jboss.tools.common.model");
			//state = b==null ? -1 : b.getState();
			//if(state == Bundle.ACTIVE) {
			synchronized (this) {
				ids.remove(r.getId());
			}
			if(monitor.isCanceled()) {
				break;
			}
			if (!isSuspended()) {
				try {
					r.run();
				} catch (Exception e) {
					if(e instanceof RuntimeException) {
						synchronized(this) {
							running = false;
						}
						throw (RuntimeException)e;
					}
					ModelPlugin.getDefault().logError("Error in job " + r.getId(), e);
				}
			}
			//}
		}
		return Status.OK_STATUS;
	}

	public static boolean isSuspended() {
		return suspended;
	}

	public static void setSuspended(boolean suspended) {
		XJob.suspended = suspended;
	}

}