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

public class HiddenFileSystemsWizardView extends AbstractListWizardView {
	public HiddenFileSystemsWizardView() {
		setHelpKey("FileSystems_ShowHide");
		this.setMessage(WizardKeys.getString("HiddenFileSystemsWizardView.Message"));
		this.setTitle(WizardKeys.getString("HiddenFileSystemsWizardView.Title"));
		this.setWindowTitle(WizardKeys.getString("HiddenFileSystemsWizardView.WindowTitle"));
	}

	protected String[] getActions() {
		return new String[]{"Hide All Jars", "Show All Jars"};
	}

	protected void internalAction(String command) {
		if("Hide All Jars".equals(command)) {
			for (int i = 0; i < boxes.length; i++)
			  if("true".equals(vs[i][2])) {
			  	boxes[i].setSelection(false);
			  	apply(i);
			  }
		} else if("Show All Jars".equals(command)) {
			for (int i = 0; i < boxes.length; i++)
			  if("true".equals(vs[i][2])) {
			  	boxes[i].setSelection(true);
			  	apply(i);
		      }
		}
	}

}
