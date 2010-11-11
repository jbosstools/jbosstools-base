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
package org.jboss.tools.common.model.ui.editors.dnd;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class DropWizardDialog extends WizardDialog {
	private static final int DO_WIDTH_VALUE = 450;
	private static final int DO_HEIGHT_VALUE = 530;
	private static final int DO_WIDTH_VALUE_LINUX = 550;

	public DropWizardDialog(Shell parentShell, IWizard newWizard) {
		super(parentShell, newWizard);
	}
	
	protected void configureShell(Shell newShell) {
		Control shell = newShell == null ? null : newShell.getParent();
		super.configureShell(newShell);
		
		int width = getPreferredWidth();
		int height = DO_HEIGHT_VALUE;
		Display d = shell != null ? shell.getDisplay() : ModelPlugin.getDefault().getWorkbench().getDisplay();
		int x = d.getBounds().width/2-width/2;
		int y = d.getBounds().height/2-height/2;
			
		if(shell != null) {
			x = shell.getBounds().x + shell.getBounds().width/2-width/2;
			if(x < 0) x = 0;
			y = shell.getBounds().y + shell.getBounds().height/2-height/2;
			if(y + height > d.getBounds().height) {
				y = d.getBounds().height - height;
			}
		}

		newShell.setBounds(x, y, width, height);
	}
	
	protected int getPreferredWidth() {
		String os_name = System.getProperty("os.name"); //$NON-NLS-1$
		if(os_name != null && os_name.indexOf("Windows") >= 0) return DO_WIDTH_VALUE; //$NON-NLS-1$
		return DO_WIDTH_VALUE_LINUX;
	}
	
	@Override
	public void updateButtons() {
		boolean canFlipToNextPage = false;
		boolean canFinish = getWizard().canFinish();
		if (getButton(IDialogConstants.BACK_ID) != null) {
			getButton(IDialogConstants.BACK_ID).setEnabled(getCurrentPage().getPreviousPage() != null);
		}
		if (getButton(IDialogConstants.NEXT_ID) != null) {
			canFlipToNextPage = getCurrentPage().canFlipToNextPage();
			getButton(IDialogConstants.NEXT_ID).setEnabled(canFlipToNextPage);
		}
		getButton(IDialogConstants.FINISH_ID).setEnabled(canFinish);
		getShell().setDefaultButton(null);
	}
	
	/*
	 *
	 * a part of https://jira.jboss.org/jira/browse/JBIDE-5876 fix
	 *
	 */
	
	@Override
	protected Button createButton(Composite parent, int id, String label,
			boolean defaultButton) {
		Button button = super.createButton(parent, id, label, defaultButton);
		if (id == IDialogConstants.FINISH_ID) {
			getShell().setDefaultButton(null);
		}
		return button;
	}
	
	@Override
	public void create() {
		super.create();
		getShell().addTraverseListener(new TraverseListener() {
			
			public void keyTraversed(TraverseEvent e) {
				if (e.character == '\r') {
					Button finishButton = getButton(IDialogConstants.FINISH_ID);
					Button cancelButton = getButton(IDialogConstants.CANCEL_ID);
					if (!finishButton.isFocusControl() && !cancelButton.isFocusControl()) {
						if (finishButton.isEnabled()) {
							buttonPressed(IDialogConstants.FINISH_ID);
						} else {
							handleShellCloseEvent();
						}
					}
				}
			}
		});
	}
	
}
