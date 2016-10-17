/******************************************************************************* 
 * Copyright (c) 2016 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.checkup.internal.model;

import java.text.ParseException;
import java.util.Date;
import java.util.concurrent.LinkedBlockingDeque;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.jboss.tools.foundation.checkup.FoundationCheckupPlugin;
import org.jboss.tools.foundation.checkup.internal.jobs.JVMProblemDetectorJob;
import org.jboss.tools.foundation.checkup.internal.jobs.ReportJob;
import org.jboss.tools.foundation.checkup.internal.log.JVMProblemLogListener;
import org.jboss.tools.foundation.checkup.internal.log.JVMProblemLogUtil;
import org.osgi.service.prefs.BackingStoreException;

public class JVMProblemModel {
	private static final String PREFERENCE_NAME="org.jboss.tools.foundation.checkup.JVMProblemDetector_NOT_TO_SHOW"; //$NON-NLS-1$

	/* Static methods and static vars*/
	private static JVMProblemModel model = new JVMProblemModel();
	public static JVMProblemModel getInstance() {
		return model;
	}
	
	// Smaller less-important member vars
	private long eclipseStartTime = getEclipseStartTimeSysprop();
	private Date currentDate = null;
	private String javaVersion = System.getProperty("java.version"); //$NON-NLS-1$;
	boolean testEnvironment = false;
	
	
	// The model that runs the show
	private UnresolvedStructure structure;
	private LinkedBlockingDeque<String> queue;
	private JVMProblemDetectorJob job;
	
	// Member vars for specific implementation (ie read log vs osgi events)
	private JVMProblemLogListener logListener;
	
	public JVMProblemModel() {
		 structure = new UnresolvedStructure(this);
		 queue = new LinkedBlockingDeque<String>();
		 job = new JVMProblemDetectorJob(this);
	}
	
	
	public void initialize() {
		addListeners();
		readModel();
		scheduleDetectorJob();
	}
	
	protected void addListeners() {
		// Add listeners for incremental updates to log or osgi framework
		logListener = new JVMProblemLogListener(this);
		Platform.addLogListener(logListener);	
	}

	/*
	 * We've been asked not to show anymore, so stop listening to events
	 */
	protected void removeListeners() {
		Platform.removeLogListener(logListener);
	}
	
	protected void readModel() {
		JVMProblemLogUtil.reaLogModel(model);
	}
	
	public void scheduleDetectorJob() {
		if( job != null )
			job.schedule();
	}
	
	public void cancelDetectorJob(){
		if( job != null ) {
			job.cancel();
		}
	}
	
	public String getJavaVersion() {
		return javaVersion;
	}
	
	public boolean isTestEnvironment() {
		return testEnvironment;
	}
	
	public void setTestEnvironment(boolean val) {
		this.testEnvironment = val;
	}
	
	/**
	 * for test purpose
	 * @param date string in yyyy-MM-dd HH:mm:ss.SSS 
	 * @throws ParseException
	 */
	public void setEclipseStartTime(String str) throws ParseException{
		Date d = JVMProblemLogUtil.getDate(str);
		if( d != null ) {
			eclipseStartTime = d.getTime();
		}
	}
	
	private long getEclipseStartTimeSysprop() {
		String ts = System.getProperty("eclipse.startTime"); //$NON-NLS-1$
		try {
			return Long.parseLong(ts);
		} catch (NumberFormatException e) {
			FoundationCheckupPlugin.logError(e);
		}
		return 0;
	}

	public long getEclipseStartTime() {
		return eclipseStartTime;
	}
	
	public void setCurrentDate(Date date) {
		if( date != null )
			currentDate = date;
	}
	
	public Date getCurrentDate() {
		return currentDate;
	}
	
	public boolean isAllowedToShow(){
		IEclipsePreferences node = InstanceScope.INSTANCE.getNode(FoundationCheckupPlugin.PLUGIN_ID);
		return node.getBoolean(PREFERENCE_NAME, true) || isTestEnvironment();
	}
	
	public void setAllowedToShow(boolean allowedToShow){
		IEclipsePreferences ep = InstanceScope.INSTANCE.getNode(FoundationCheckupPlugin.PLUGIN_ID);
		try {
			ep.putBoolean(PREFERENCE_NAME, allowedToShow);
			ep.flush();
		} catch (BackingStoreException e) {
			FoundationCheckupPlugin.logError(e);
		}
		
		if(!allowedToShow){
			removeListeners();
		} 
	}

	
	public static String getPreferenceName() {
		return PREFERENCE_NAME;
	}

	public UnresolvedStructure getStructure() {
		return structure;
	}

	public LinkedBlockingDeque<String> getQueue() {
		return queue;
	}

	public JVMProblemDetectorJob getJob() {
		return job;
	}

	public boolean needsReport() {
		return getStructure().isNeedReport();
	}
	
	public void report(long delay) {
		ReportJob job = new ReportJob(this);
		job.schedule(delay);
	}
}
