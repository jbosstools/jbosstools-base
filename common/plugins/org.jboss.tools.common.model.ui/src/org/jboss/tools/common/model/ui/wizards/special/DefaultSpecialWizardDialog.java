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
package org.jboss.tools.common.model.ui.wizards.special;

import org.jboss.tools.common.model.ui.action.CommandBar;
import org.jboss.tools.common.model.ui.action.CommandBarLayout;
import org.jboss.tools.common.model.ui.action.CommandBarListener;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import org.jboss.tools.common.meta.action.impl.SpecialWizardSupport;
import org.jboss.tools.common.meta.impl.XModelEntityImpl;
import org.jboss.tools.common.model.ui.ModelUIImages;

public class DefaultSpecialWizardDialog extends TitleAreaDialog implements CommandBarListener {
	public static boolean hideHelp = XModelEntityImpl.hideHelp;
	private DefaultSpecialWizard wizard;
	private String[] commands = new String[0];
	private CommandBar commandBar = null;
	
	private int pageWidth = 490;
	private int pageHeight = 225;
	
	private Point minimumSize = null;
	private Point maximumSize = null;
	
	public DefaultSpecialWizardDialog(Shell shell) {
		super(shell);
	}
	
	protected Control stepPage;
	
	protected Control createPage(Composite parent) {
		if (stepPage!=null) {
			stepPage.dispose();
			stepPage = null;
		}
		ISpecialWizardStep wizardStep = wizard.getWizardStep();
		this.setTitleImage(ModelUIImages.getImageDescriptor(ModelUIImages.WIZARD_DEFAULT).createImage(null));
		return wizardStep.createControl(parent);
	}
	
	public void updateDialogArea() {
		stepPage = createPage((Composite)pageArea);
		//stepPage.setBackground(new Color(null,128,255,255));
		Point size = stepPage.computeSize(SWT.DEFAULT, SWT.DEFAULT);
		size.x = Math.max(size.x,pageWidth);
		size.y = Math.max(size.y,pageHeight);
		GridData gd = (GridData)stepPage.getLayoutData();
		if (gd==null) gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = size.y;
		stepPage.setLayoutData(gd);
		((Composite)pageArea).layout(true);
		updateSizeForPage();
	}
	
	protected Control pageArea;
	protected Control createPageArea(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout(1, false);
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.horizontalSpacing = 0;
		gridLayout.verticalSpacing = 0;
		composite.setLayout(gridLayout);
		return composite;		
	}
	
	protected Composite dialogArea;
	protected Control createDialogArea(Composite parent) {
		dialogArea = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout(1, false);
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.horizontalSpacing = 0;
		gridLayout.verticalSpacing = 0;
		dialogArea.setLayout(gridLayout);
		//composite.setBackground(new Color(null, 255,0,255));

		// Build the separator line
		Label dialogAreaSeparator = new Label(dialogArea, SWT.HORIZONTAL | SWT.SEPARATOR);
		dialogAreaSeparator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		// Build the page area
		pageArea = createPageArea(dialogArea);
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.widthHint = pageWidth;
		gd.heightHint = pageHeight;
		pageArea.setLayoutData(gd);
		
		gd = new GridData(GridData.FILL_BOTH);
		gd.grabExcessVerticalSpace = true;
		
		dialogArea.setLayoutData(gd);
		return dialogArea;
	}
	
	protected Control createButtonBar(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout(1, false);
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.horizontalSpacing = 0;
		gridLayout.verticalSpacing = 0;
		composite.setLayout(gridLayout);
		//composite.setBackground(new Color(null, 0,255,255));
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		//gd.heightHint = commandBarHeight;
		composite.setLayoutData(gd);

		// Build the separator line
		Label titleBarSeparator = new Label(composite, SWT.HORIZONTAL | SWT.SEPARATOR);
		titleBarSeparator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		// Build the command bar
		commandBar = new CommandBar();
		CommandBarLayout cbl = new CommandBarLayout();
		cbl.buttonHeight = convertHorizontalDLUsToPixels(20);
		cbl.buttonWidth = convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_WIDTH);
		cbl.gap = convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_MARGIN);
		cbl.left = 10;		
		cbl.right = 10;
		cbl.top = 11;
		cbl.bottom = 10;
		commandBar.setLayout(cbl);
		Control control = commandBar.createControl(composite);
		commandBar.addCommandBarListener(this);
		commandBar.getLayout().alignment = SWT.RIGHT;
		gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = commandBar.getControl().computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
		control.setLayoutData(gd);
		//control.setBackground(new Color(null, 0,0,255));

		return composite;
	}
	
	public void updateButtonsBar(String[] newCommands) {
		newCommands = revalidateHelpInCommands(newCommands);
		if(!equal(commands, newCommands)) {
			commandBar.setCommands(commands = newCommands);
			commandBar.update();
			((Composite)commandBar.getControl()).layout();
		}
	}
	
	private String[] revalidateHelpInCommands(String[] newCommands) {
		if(!hideHelp || newCommands.length == 0 || !newCommands[newCommands.length - 1].equals(SpecialWizardSupport.HELP)) return newCommands;
		String[] s = new String[newCommands.length - 1];
		System.arraycopy(newCommands, 0, s, 0, s.length);
		return s;
	}

	private void setShellSize(int width, int height) {
		// clip for 3/4 desktop
		Rectangle displayArea = this.getShell().getDisplay().getClientArea();
		if ((this.getMinimumSize()!=null)&&(this.getMinimumSize().x>displayArea.width)) {
			width = this.getMinimumSize().x;
		} else {
			width = Math.min(width,displayArea.width*3/4);
		}
		if ((this.getMinimumSize()!=null)&&(this.getMinimumSize().y>displayArea.width)) {
			height = this.getMinimumSize().y;
		} else {
			height = Math.min(height,displayArea.height*3/4); 
		}
		
		if ((this.getMinimumSize()!=null)&&(this.getMinimumSize().x!=SWT.DEFAULT)&&(this.getMinimumSize().x>width)) {
			width = this.getMinimumSize().x;
		}
		if ((this.getMinimumSize()!=null)&&(this.getMinimumSize().y!=SWT.DEFAULT)&&(this.getMinimumSize().y>height)) {
			height = this.getMinimumSize().y;
		}
		if ((this.getMaximumSize()!=null)&&(this.getMaximumSize().x!=SWT.DEFAULT)&&(this.getMaximumSize().x<width)) {
			width = this.getMaximumSize().x;
		}
		if ((this.getMaximumSize()!=null)&&(this.getMaximumSize().y!=SWT.DEFAULT)&&(this.getMaximumSize().y<height)) {
			height = this.getMaximumSize().y;
		}
		getShell().setSize(width, height);
		constrainShellSize();
		
	}

	private Point calculatePageSizeDelta() {
		Control pageControl = stepPage;
		if (pageControl == null) return new Point(0, 0);

		Point contentSize =	pageControl.computeSize(SWT.DEFAULT, SWT.DEFAULT, true);
		contentSize.x = Math.max(contentSize.x,pageWidth);
		contentSize.y = Math.max(contentSize.y,pageHeight);
		
//		Rectangle displayArea = this.getShell().getDisplay().getClientArea();
//		Point displaySize = new Point(displayArea.width, displayArea.height);
		// temp bugfix
		//contentSize.x = Math.min(contentSize.x,displaySize.x*3/4); 
		//contentSize.y = Math.min(contentSize.y,displaySize.y*3/4); 
		
		Rectangle rect = ((Composite)pageArea).getClientArea();
		Point containerSize = new Point(rect.width, rect.height);

		return new Point(
			Math.max(0, contentSize.x - containerSize.x),
			Math.max(0, contentSize.y - containerSize.y));
	}

	private void updateSizeForPage() {
		Point delta = calculatePageSizeDelta();

		if (delta.x > 0 || delta.y > 0) {
			// increase the size of the shell 
			Shell shell = getShell();
			Point shellSize = shell.getSize();
			setShellSize(shellSize.x + delta.x, shellSize.y + delta.y);
		}
	}
	
	private boolean equal(String[] s1, String[] s2) {
		if(s1 == null || s2 == null || s1.length != s2.length) return false;
		for (int i = 0; i < s1.length; i++) if(!s1[i].equals(s2[i])) return false;
		return true;
	}
	
	public boolean close() {
		if(wizard != null) wizard.stopValidator();
		boolean b = super.close();
		wizard = null;
		if (commandBar!=null) commandBar.dispose();
		commandBar = null;
		return b;	
	}
	
	// CommandBarListener
	public void action(String command) {
		wizard.action(command);
	}	

	public Control getButtonBar() {
		return buttonBar;
	}

	public DefaultSpecialWizard getWizard() {
		return wizard;
	}
	public void setWizard(DefaultSpecialWizard wizard) {
		this.wizard = wizard;
	}

	public Point getMinimumSize() {
		return minimumSize;
	}
	public void setMinimumSize(Point point) {
		minimumSize = point;
	}
	public Point getMaximumSize() {
		return maximumSize;
	}
	public void setMaximumSize(Point point) {
		maximumSize = point;
	}
	
	public CommandBar getCommandBar() {
		return commandBar;
	}
	
}
