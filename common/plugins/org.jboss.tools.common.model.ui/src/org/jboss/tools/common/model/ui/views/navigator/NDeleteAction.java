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

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

public class NDeleteAction extends NSelectionAction {
	public static final String ID = PlatformUI.PLUGIN_ID + ".DeleteResourceAction";//$NON-NLS-1$

	public NDeleteAction(Shell shell) {
		super("Delete"); //$NON-NLS-1$
		setToolTipText("Delete"); //$NON-NLS-1$
		setId(ID);
		if (shell == null) {
			throw new IllegalArgumentException();
		}
///		ModelUIPlugin.getDefault().getWorkbench().getHelpSystem().setHelp(this, IIDEHelpContextIds.DELETE_RESOURCE_ACTION);
	}

	protected String getActionPath() {
		return "DeleteActions.Delete";
	}


}
