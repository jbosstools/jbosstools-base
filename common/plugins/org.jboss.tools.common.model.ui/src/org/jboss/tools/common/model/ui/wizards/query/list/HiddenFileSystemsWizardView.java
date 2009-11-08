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
package org.jboss.tools.common.model.ui.wizards.query.list;

import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.ui.ModelUIMessages;

public class HiddenFileSystemsWizardView extends AbstractListWizardView {
	public HiddenFileSystemsWizardView() {
		setHelpKey("FileSystems_ShowHide"); //$NON-NLS-1$
		this.setMessage(WizardKeys.getString("HiddenFileSystemsWizardView.Message")); //$NON-NLS-1$
		this.setTitle(WizardKeys.getString("HiddenFileSystemsWizardView.Title")); //$NON-NLS-1$
		this.setWindowTitle(WizardKeys.getString("HiddenFileSystemsWizardView.WindowTitle")); //$NON-NLS-1$
	}

	protected String[] getActions() {
		return new String[]{ModelUIMessages.HiddenFileSystemsWizardView_HideAllJars, ModelUIMessages.HiddenFileSystemsWizardView_ShowAllJars};
	}

	protected void internalAction(String command) {
		if(ModelUIMessages.HiddenFileSystemsWizardView_HideAllJars.equals(command)) {
			for (int i = 0; i < boxes.length; i++)
			  if("true".equals(vs[i][2])) { //$NON-NLS-1$
			  	boxes[i].setSelection(false);
			  	apply(i);
			  }
		} else if(ModelUIMessages.HiddenFileSystemsWizardView_ShowAllJars.equals(command)) {
			for (int i = 0; i < boxes.length; i++)
			  if("true".equals(vs[i][2])) { //$NON-NLS-1$
			  	boxes[i].setSelection(true);
			  	apply(i);
		      }
		}
	}

}
