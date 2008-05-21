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
package org.jboss.tools.common.gef.action;

import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.internal.WorkbenchImages;

public abstract class DiagramCutAction extends DiagramSelectionAction {

	public DiagramCutAction(IWorkbenchPart editor) {
		super(editor);
	}

	/**
	 * Initializes this action's text and images.
	 */
	protected void init() {
		super.init();
		//setText(GEFMessages.CutAction_Label);
		//setToolTipText(GEFMessages.CutAction_Tooltip);
		setId(ActionFactory.CUT.getId());
		setActionDefinitionId(ActionFactory.CUT.getId());
		setHoverImageDescriptor(
			WorkbenchImages.getImageDescriptor(ISharedImages.IMG_TOOL_CUT));
		setImageDescriptor(
			WorkbenchImages.getImageDescriptor(
				ISharedImages.IMG_TOOL_CUT));
		setDisabledImageDescriptor(
			WorkbenchImages.getImageDescriptor(
				ISharedImages.IMG_TOOL_CUT_DISABLED));
		setAccelerator(convertAccelerator("CTRL+X"));
		setEnabled(false);
	}

}
