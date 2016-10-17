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
package org.jboss.tools.foundation.checkup.internal.log;

import org.eclipse.core.runtime.ILogListener;
import org.eclipse.core.runtime.IStatus;
import org.jboss.tools.foundation.checkup.internal.model.JVMProblemModel;

public class JVMProblemLogListener implements ILogListener {
	private JVMProblemModel model;
	public JVMProblemLogListener(JVMProblemModel model) {
		this.model = model;
	}
	private void processStatus(IStatus status){
		Throwable exception = status.getException();
		String message = status.getMessage();
		if(message != null){
			try {
				model.getQueue().put(message);
			} catch (InterruptedException e) {
				// do nothing
			}
		}
		if(exception != null){
			message = exception.getMessage();
			if(message != null){
				try {
					model.getQueue().put(message);
				} catch (InterruptedException e) {
					// do nothing
				}
			}
			for(IStatus child : status.getChildren()){
				processStatus(child);
			}
		}
	}

	public void logging(IStatus status, String plugin) {
		processStatus(status);
		model.scheduleDetectorJob();
	}
}
