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

public abstract class DiagramDeleteAction extends DiagramSelectionAction {

	public static final String ID = ActionFactory.DELETE.getId();

	public DiagramDeleteAction(IWorkbenchPart editor) {
		super(editor);
	}

	/**
	 * Initializes this action's text and images.
	 */
	final protected void init() {
		super.init();
		setText(GEFMessages.DeleteAction_Label);
		setToolTipText(GEFMessages.DeleteAction_Tooltip);
		setId(ActionFactory.DELETE.getId());
		setHoverImageDescriptor(
			WorkbenchImages.getImageDescriptor(ISharedImages.IMG_TOOL_DELETE));
		setImageDescriptor(
			WorkbenchImages.getImageDescriptor(
				ISharedImages.IMG_TOOL_DELETE));
		setDisabledImageDescriptor(
			WorkbenchImages.getImageDescriptor(
				ISharedImages.IMG_TOOL_DELETE_DISABLED));
		setEnabled(false);
	}

}


