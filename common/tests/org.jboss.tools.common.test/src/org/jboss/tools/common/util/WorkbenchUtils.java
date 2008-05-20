/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 

package org.jboss.tools.common.util;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.jboss.tools.common.CommonPlugin;

/**
 * @author eskimo
 *
 */
public class WorkbenchUtils {
	
	static public IWizard findWizardByDefId(String definitionId) {
		 IWizardDescriptor aWizardDescr = 
			 getWorkbench().getNewWizardRegistry()
					.findWizard(definitionId);
			IWorkbenchWizard aWizard=null;
			try {
				aWizard = aWizardDescr.createWizard();
			} catch (CoreException e) {
				CommonPlugin.getPluginLog().logError("Cannot create IWorkbenchWizard instance",e);
			}
			return aWizard;
	}
	
	public static IWorkbench getWorkbench() {
		return PlatformUI.getWorkbench();
	}

	/**
	 * @return
	 */
	public static IWorkbenchPage getWorkbenchActivePage() {
		// TODO Auto-generated method stub
		return getWorkbench().getActiveWorkbenchWindow().getActivePage();
	}
}
