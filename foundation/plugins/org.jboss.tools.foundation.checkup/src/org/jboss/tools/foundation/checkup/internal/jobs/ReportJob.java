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

import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.progress.UIJob;
import org.jboss.tools.foundation.checkup.FoundationCheckupPlugin;
import org.jboss.tools.foundation.checkup.JVMProblemDetectorMessages;
import org.jboss.tools.foundation.checkup.dialog.UnresolvedClassesDialog;
import org.jboss.tools.foundation.checkup.dialog.UnresolvedModulesDialog;
import org.jboss.tools.foundation.checkup.internal.model.JVMProblemModel;
import org.jboss.tools.foundation.checkup.internal.model.UnresolvedClass;
import org.jboss.tools.foundation.checkup.internal.model.UnresolvedModule;
import org.jboss.tools.foundation.checkup.internal.model.UnresolvedStructure;

public class ReportJob extends UIJob{
	private JVMProblemModel model;
	private UnresolvedStructure structure;
	private String javaVersion;
	public ReportJob(JVMProblemModel model) {
		super(JVMProblemDetectorMessages.SHOW_WARNING_DIALOG_JOB_TITLE);
		this.model = model;
		this.structure = model.getStructure();
		this.javaVersion = model.getJavaVersion();
	}

	public IStatus runInUIThread(IProgressMonitor monitor) {
		if(!UnresolvedModulesDialog.showing){
			// call from UI Thread
			List<UnresolvedModule> modules = structure.getUnresolvedModules();
			
			if(modules.size() > 0){
				UnresolvedModulesDialog dialog = new UnresolvedModulesDialog(Display.getDefault().getActiveShell(), modules, javaVersion);
				dialog.open();
				setAllowedToShow(dialog.showNextTime());
			}
		}
		if(!UnresolvedClassesDialog.showing){
			// call from UI Thread
			List<UnresolvedClass> classes = structure.getUnresolvedClasses();
			
			if(classes.size() > 0){
				UnresolvedClassesDialog dialog = new UnresolvedClassesDialog(Display.getDefault().getActiveShell(), classes, javaVersion);
				dialog.open();
				setAllowedToShow(dialog.showNextTime());
			}
		}
		Status status = new Status(Status.OK, FoundationCheckupPlugin.PLUGIN_ID, "");
		return status;
	}
	
	private void setAllowedToShow(boolean allowedToShow){
		model.setAllowedToShow(allowedToShow);
	}
}