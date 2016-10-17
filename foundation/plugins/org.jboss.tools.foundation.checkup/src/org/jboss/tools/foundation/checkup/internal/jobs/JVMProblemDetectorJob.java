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
package org.jboss.tools.foundation.checkup.internal.jobs;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.jboss.tools.foundation.checkup.FoundationCheckupPlugin;
import org.jboss.tools.foundation.checkup.JVMProblemDetectorMessages;
import org.jboss.tools.foundation.checkup.internal.log.LineScanner;
import org.jboss.tools.foundation.checkup.internal.model.JVMProblemModel;

public class JVMProblemDetectorJob extends Job{
	private static final long WAIT_TIME_AFTER_EVENT = 3000;
	
	private JVMProblemModel model;
	public JVMProblemDetectorJob(JVMProblemModel model) {
		super(JVMProblemDetectorMessages.JOB_TITLE);
		setSystem(true);
		setPriority(LONG);
		this.model = model;
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		readQueue();
		Status status = new Status(Status.OK, FoundationCheckupPlugin.PLUGIN_ID, "");
		return status;
	}

	void readQueue(){
		LineScanner ls = new LineScanner(model);
		try{
			while(model.isAllowedToShow() && !model.getQueue().isEmpty()){
				String message = model.getQueue().take();
				BufferedReader reader = null;
				try{
					reader = new BufferedReader(new StringReader(message)); 
					while (true) {
						String line0 = reader.readLine();
						if (line0 == null)
							break;
						String line = line0.trim();
						ls.scanLine(line);
					}
				}finally{
					if(reader != null){
						reader.close();
					}
				}
				
				if(model.getStructure().isNeedReport()){
					model.report(WAIT_TIME_AFTER_EVENT);
				}
			}
		} catch (IOException e){
			FoundationCheckupPlugin.logError(e);
		} catch (InterruptedException e) {
			// do nothing
		}
	}
}