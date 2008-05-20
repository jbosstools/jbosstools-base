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
package org.jboss.tools.common.model.ui.views.navigator;

import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

public class NPasteAction extends NSelectionAction {
	public static final String ID = PlatformUI.PLUGIN_ID + ".PasteAction";//$NON-NLS-1$

	public NPasteAction(Shell shell, Clipboard clipboard) {
		super("Paste"); //$NON-NLS-1$
		setToolTipText("Paste");
		setId(NPasteAction.ID);
///		WorkbenchHelp.setHelp(this, INavigatorHelpContextIds.PASTE_ACTION);
	}

	protected String getActionPath() {
		return "CopyActions.Paste";
	}
}
