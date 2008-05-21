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

public class NCopyAction extends NSelectionAction {
	public static final String ID = PlatformUI.PLUGIN_ID + ".CopyAction"; //$NON-NLS-1$
	private NPasteAction pasteAction;

	public NCopyAction(Shell shell, Clipboard clipboard) {
		super("Copy"); //$NON-NLS-1$
		setToolTipText("Copy"); //$NON-NLS-1$
		setId(NCopyAction.ID);
///		WorkbenchHelp.setHelp(this, INavigatorHelpContextIds.COPY_ACTION);
	}

	public NCopyAction(Shell shell, Clipboard clipboard, NPasteAction pasteAction) {
		this(shell, clipboard);
		this.pasteAction = pasteAction;
	}

	protected String getActionPath() {
		return "CopyActions.Copy";
	}
	
	public void run() {
		super.run();
		if (pasteAction != null && pasteAction.getStructuredSelection() != null) 
			pasteAction.selectionChanged(pasteAction.getStructuredSelection());
	}

}
