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
package org.jboss.tools.common.verification.ui.vrules.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.preference.*;
import org.eclipse.jface.viewers.*;

import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.ui.action.*;

public class VRulesConfigurationAction extends AbstractModelActionDelegate {

	protected void safeSelectionChanged(IAction action, ISelection selection) {
		action.setEnabled(computeEnabled());
	}
	
	public void doRun() throws Exception {
		new VRulesPreferenceDialog().open();
	}

	protected boolean computeEnabled() {
		return true; 
	}
	
	class VRulesPreferenceDialog extends PreferenceDialog {
		public VRulesPreferenceDialog() {
			super(ModelPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell(), 
			ModelPlugin.getDefault().getWorkbench().getPreferenceManager());
			setSelectedNodePreference("org.jboss.tools.common.verification.ui");
		}
	}
	
}
