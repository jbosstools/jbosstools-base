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
package org.jboss.tools.common.model.ui.wizards.query;

import org.jboss.tools.common.model.ui.action.CommandBar;
import org.jboss.tools.common.model.ui.action.CommandBarLayout;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import org.jboss.tools.common.meta.impl.XModelEntityImpl;
import org.jboss.tools.common.model.ui.ModelUIImages;

public class AbstractQueryDialog extends TitleAreaDialog implements IQueryDialog {
	public static boolean hideHelp = XModelEntityImpl.hideHelp;
	private AbstractQueryWizardView view;
	private int pageWidth = 490;
	private int pageHeight = 225;
	
	public AbstractQueryDialog(Shell shell) {
		super(shell);
	}
	
	public void setView(AbstractQueryWizardView view) {
		this.view = view;
	}

	protected Control createDialogArea(Composite parent) {
		setTitleImage(ModelUIImages.getImageDescriptor(ModelUIImages.WIZARD_DEFAULT).createImage(null));

		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout(1, false);
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.horizontalSpacing = 0;
		gridLayout.verticalSpacing = 0;
		composite.setLayout(gridLayout);

		Label dialogAreaSeparator = new Label(composite, SWT.HORIZONTAL | SWT.SEPARATOR);
		dialogAreaSeparator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		Control pageArea = view.createControl(composite);
		GridData gd = new GridData(GridData.FILL_BOTH);
		pageArea.setLayoutData(gd);
		
		gd = new GridData(GridData.FILL_BOTH);
		
		Point p = view.getPreferredSize();
		gd.widthHint = (p == null) ? pageWidth : p.x;
		gd.heightHint = (p == null) ? pageHeight : p.y;
		composite.setLayoutData(gd);
		return composite;
	}
	
	protected Control createButtonBar(Composite parent) {
		CommandBar commandBar = view.getCommandBar();
		commandBar.addCommandBarListener(view);
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout(1, false);
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.horizontalSpacing = 0;
		gridLayout.verticalSpacing = 0;
		composite.setLayout(gridLayout);
		GridData gd = new GridData(GridData.FILL_BOTH);
		composite.setLayoutData(gd);

		Label titleBarSeparator = new Label(composite, SWT.HORIZONTAL | SWT.SEPARATOR);
		titleBarSeparator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		CommandBarLayout cbl = new CommandBarLayout();
		cbl.buttonHeight = convertHorizontalDLUsToPixels(20);
		cbl.buttonWidth = convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_WIDTH);
		cbl.gap = convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_MARGIN);
		cbl.left = 10;		
		cbl.right = 10;
		cbl.top = 11;
		cbl.bottom = 10;
		commandBar.setLayout(cbl);
		commandBar.setCommands(getCommands());
		commandBar.setDefaultCommand(view.getDefaultCommand());
		Control control = commandBar.createControl(composite);
		commandBar.getLayout().alignment = SWT.RIGHT;
		gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = commandBar.getControl().computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
		control.setLayoutData(gd);
		if(view.getHelpKey() == null)
		  commandBar.setEnabled(AbstractQueryWizardView.HELP, false);
		view.updateBar();
		return composite;
	}
	
	protected String[] getCommands() {
		String[] s = view.getCommands();
		if(s.length == 0 || !s[s.length - 1].equals(AbstractQueryWizardView.HELP)) {
			String[] s1 = new String[s.length + 1];
			for (int i = 0; i < s.length; i++) s1[i] = s[i];
			s1[s.length] = AbstractQueryWizardView.HELP;
			s = s1;
		}
		if(hideHelp) {
			String[] s1 = new String[s.length - 1];
			System.arraycopy(s, 0, s1, 0, s1.length);
			return s1;
		}
		return s;
	}

	public Dialog getDialog() {
		return this;
	}
	
}
