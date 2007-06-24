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
package org.jboss.tools.common.model.ui.wizard.newfile;

import org.jboss.tools.common.model.ui.util.ExtensionPointUtils;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.INewWizard;
import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class NewClassCreationWizard implements SpecialWizard {
	XModelObject object = null;
	
	public void setObject(Object object) {
		this.object = (XModelObject)object;
	}

	public int execute() {
		INewWizard wizard = ExtensionPointUtils.findNewWizardsItem(
			"org.eclipse.jdt.ui",
			"org.eclipse.jdt.ui.wizards.NewClassCreationWizard"
		);
		if (wizard != null) {
			wizard.init(
				ModelUIPlugin.getDefault().getWorkbench(),
				new StructuredSelection(object) 
			);
			WizardDialog dialog = new WizardDialog(ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell(), wizard);
			dialog.open();  
		} else {
			ModelUIPlugin.log("Unable to create wizard 'org.eclipse.jdt.ui.wizards.NewClassCreationWizard'.", new ClassNotFoundException());
		}
		return 0;
	}
}
