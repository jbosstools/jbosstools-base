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
import org.eclipse.gef.internal.GEFMessages;

/**
 * An action to delete selected objects.
 */
public abstract class DiagramPasteAction extends DiagramSelectionAction {

	public DiagramPasteAction(IWorkbenchPart editor) {
		super(editor);
	}

	/**
	 * Initializes this action's text and images.
	 */
	protected void init() {
		super.init();
		setText(GEFMessages.PasteAction_Label);
		setToolTipText(GEFMessages.PasteAction_Tooltip);
		setId(ActionFactory.PASTE.getId());
		setActionDefinitionId(ActionFactory.PASTE.getId());
		setHoverImageDescriptor(
			WorkbenchImages.getImageDescriptor(
				ISharedImages.IMG_TOOL_PASTE));
		setImageDescriptor(
			WorkbenchImages.getImageDescriptor(
				ISharedImages.IMG_TOOL_PASTE));
		setDisabledImageDescriptor(
			WorkbenchImages.getImageDescriptor(
				ISharedImages.IMG_TOOL_PASTE_DISABLED));
		setAccelerator(convertAccelerator("CTRL+V"));
	
		setEnabled(false);
	}

}


