/******************************************************************************* 
 * Copyright (c) 2015 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.foundation.checkup;

import org.eclipse.ui.IStartup;
import org.jboss.tools.foundation.checkup.internal.model.JVMProblemModel;


public class JVMProblemDetector implements IStartup {
	private JVMProblemModel model;
	public JVMProblemDetector(){
		super();
		model = JVMProblemModel.getInstance();
	}
	
	
	public void earlyStartup() {
		if(model.isAllowedToShow()){
			model.initialize();
		}
	}
}