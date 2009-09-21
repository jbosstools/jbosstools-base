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
package org.jboss.tools.common.model.ui.problem;

import java.util.Iterator;
import java.util.List;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IconAndMessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;


public class ProblemDialog extends IconAndMessageDialog {

	private String title;
	private List actions;

	public ProblemDialog(Shell shell, Problem error) {
		super(shell);
		setShellStyle(SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
		this.title = (error.getTitle()!=null)?error.getTitle():""; //$NON-NLS-1$
		this.message = error.getMessage();
		if (error.getLine()!=Problem.NONE) this.message = this.message + " at line:" + error.getLine();
		if (error.getColumn()!=Problem.NONE) this.message = this.message + " column:" + error.getColumn();
		this.actions = error.getActions();
	}

	protected Image getImage() {
		return Display.getDefault().getSystemImage(SWT.ICON_ERROR);
	}

	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		shell.setText(title);
	}
	
	protected Control createDialogArea(Composite parent) {
		return createMessageArea(parent);
	}
	

	protected void createButtonsForButtonBar(Composite parent) {
		if (actions==null || actions.size()==0) {
			createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,	true);
		} else {
			Iterator i = actions.iterator();
			while (i.hasNext()) {
				ProblemDialogAction action = (ProblemDialogAction)i.next();
				createButton(parent, action.getDialogId(), action.getText(), action.isDefault());
			}
		}
	}
	
	protected void buttonPressed(int buttonId) {
		if (actions!=null && actions.size()>0) {
			Iterator i = actions.iterator();
			while(i.hasNext()) {
				ProblemDialogAction action = (ProblemDialogAction)i.next();
				if (buttonId==action.getDialogId()) {
					action.run();
				}
			}
		}
		super.buttonPressed(buttonId);
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#open()
	 */
	public int open() {
		return super.open();
	}

}
