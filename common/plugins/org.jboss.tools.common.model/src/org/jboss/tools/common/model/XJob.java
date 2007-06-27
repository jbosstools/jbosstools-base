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
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.internal.decorators.DecoratorManager;
import org.eclipse.ui.progress.UIJob;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.osgi.framework.Bundle;

public class XJob extends Job {
	public static Object FAMILY_XJOB = new Object();

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
			if(stop) {
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
				System.out.println(js[i].getName());
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
		super("RedHat Model Update");
		setSystem(true);
	}

	public boolean belongsTo(Object family) {
		return family == FAMILY_XJOB;
	}

	protected IStatus run(IProgressMonitor monitor) {
		while(true) {
			XRunnable r = null;
			synchronized (this) {
				if(list.size() == 0) break;
				r = list.remove(0);
			}
			try {
				monitor.subTask(r.getId());
				int state = 0;
				try {
					Bundle b = Platform.getBundle("org.jboss.tools.common.model");
					state = b.getState();
				} catch (Exception e2) {
					//ignore, bundle is not active
				}
				if(state == Bundle.ACTIVE) {
					r.run();
				}
			} catch (Exception e) {
				int state = 0;
				try {
					Bundle b = Platform.getBundle("org.jboss.tools.common.model");
					state = b.getState();
				} catch (Exception e2) {
					//ignore, bundle is not active
				}
				if(state == Bundle.ACTIVE) {
					ModelPlugin.getPluginLog().logError(e);
				}
			}
			synchronized (this) {
				ids.remove(r.getId());
			}
		}
		return Status.OK_STATUS;
	}
	
	void addRunnableInternal(XRunnable runnable) {
		synchronized (this) {
			if(ids.contains(runnable.getId())) return;
			ids.add(runnable.getId());
			list.add(runnable);
		}
		if(getState() == Job.NONE) {
			schedule(1000);
		}
	}

	void addRunnableInternalWithPriority(XRunnable runnable) {
		synchronized (this) {
			if(ids.contains(runnable.getId())) return;
			ids.add(runnable.getId());
			list.add(0, runnable);
		}
		if(getState() == Job.NONE) {
			schedule(0);
		}
	}

}
