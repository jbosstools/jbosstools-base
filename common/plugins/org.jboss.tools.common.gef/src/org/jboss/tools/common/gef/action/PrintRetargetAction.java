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
import org.eclipse.ui.actions.RetargetAction;


import org.eclipse.jface.resource.ImageDescriptor;
import org.jboss.tools.common.gef.Messages;



public class PrintRetargetAction extends RetargetAction{

	static final String ACTION_ID = "Print_Diagram"; //$NON-NLS-1$

	public PrintRetargetAction() {
		super(ACTION_ID,Messages.PrintRetargetAction_Name);
		setToolTipText(Messages.PrintRetargetAction_Name);
		setImageDescriptor(ImageDescriptor.createFromFile(getClass(), "icons/print.gif")); //$NON-NLS-1$
	}

}