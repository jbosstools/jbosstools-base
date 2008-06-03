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
package org.jboss.tools.common.model.ui.views.palette.editor;

import org.jboss.tools.common.model.ui.wizards.query.list.AbstractTreeWizardView;

public class HiddenPaletteTabsWizardView extends AbstractTreeWizardView {
	public HiddenPaletteTabsWizardView() {
		setHelpKey("SharablePalette_HiddenTabs");
	}

	protected String[] getActions() {
		return new String[]{"Show All", "Hide All"};
	}

	protected void internalAction(String command) {
		if("Hide All".equals(command)) {
			disableAll();
		} else if("Show All".equals(command)) {
			enableAll();
		}
	}

}
