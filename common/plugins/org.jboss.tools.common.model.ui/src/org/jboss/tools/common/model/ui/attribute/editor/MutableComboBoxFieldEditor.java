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
package org.jboss.tools.common.model.ui.attribute.editor;

import java.util.Properties;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.util.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

import org.jboss.tools.common.meta.action.SpecialWizard;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class MutableComboBoxFieldEditor extends ComboBoxFieldEditor implements IMutableFieldEditor {
	private Button changeButton;
	private String changeButtonText = "New...";
	private Composite composite;
	
	private SpecialWizard change;
	
	public MutableComboBoxFieldEditor() {
	}
	
	public MutableComboBoxFieldEditor(IWidgetSettings settings) {
		super(settings);
	}
	
	public void setChange(SpecialWizard change) {
		this.change = change;
	}

	protected void adjustForNumColumns(int numColumns) {
		GridData gd = (GridData)getComboField().getLayoutData();
		gd.horizontalSpan = numColumns - 2;
		// We only grab excess space if we have to
		// If another field editor has more columns then
		// we assume it is setting the width.
		gd.grabExcessHorizontalSpace = gd.horizontalSpan == 1;
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		getLabelComposite(parent);
		Control control = getTextChangeControl(parent);
		control.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
	}
	
	protected Composite getTextChangeControl(Composite parent) {
		//if (composite == null) 
		createTextChangeControl(parent);
		return composite;
	}
	
	protected Control createTextChangeControl(Composite parent) {
		GridData gd;
		Control control;
		if(composite == null)
			composite = new Composite(parent, SWT.NONE);
		composite.setBackground(parent.getBackground());
		GridLayout gridLayout = new GridLayout(3, false);
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.horizontalSpacing = 0;
		gridLayout.verticalSpacing = 0;
		composite.setLayout(gridLayout);
		
		Control textControl = getComboControl(composite);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		textControl.setLayoutData(gd);

		control = new Label(composite, SWT.NONE);
		control.setBackground(parent.getBackground());
		gd = new GridData();
		gd.widthHint = 5;
		control.setLayoutData(gd);
		
		control = getChangeControl(composite);
		gd = new GridData();
		gd.widthHint = convertHorizontalDLUsToPixels(control, IDialogConstants.BUTTON_WIDTH);
		gd.heightHint = textControl.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
		control.setLayoutData(gd);

/*
		// init data
		if (valueProvider!=null) {
			String value = valueProvider.getStringValue(Boolean.TRUE.booleanValue());
			getTextField().setText(value);
			if (getLabelAction()!=null) {
				getLabelAction().setEnabled((value!=null && value.length()>0));
			}
		}
*/		
		return composite;
	}


	protected Button getChangeControl(Composite parent) {
		if (changeButton == null) {
			int style = getSettings().getStyle("Button.Style");
			if (style==SWT.DEFAULT) style = SWT.NONE;
			if (style==0) style = SWT.PUSH;
			Color bg = getSettings().getColor("Button.Background");
			Color fg = getSettings().getColor("Button.Foreground");
			Font font = getSettings().getFont("Button.Font");
			changeButton = new Button(parent, style);
			changeButton.setFont(font);
			changeButton.setBackground(bg);
			changeButton.setBackground(bg);
			changeButton.setForeground(fg);

			//changeButton = new Button(parent, SWT.PUSH);
			if (changeButtonText == null)
				changeButtonText = JFaceResources.getString("openChange");//$NON-NLS-1$
			changeButton.setText(changeButtonText);
			changeButton.setFont(parent.getFont());
			changeButton.addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent evt) {
					String newValue = changePressed();
					if (newValue != null) {
						getComboField().setItems(getTags());
						setStringValue(newValue);
					}
				}
			});
			changeButton.addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent event) {
					changeButton = null;
				}
			});
		} else {
			checkParent(changeButton, parent);
		}
		return changeButton;
	}

	public int getNumberOfControls() {
		return 3;
	}

	protected Shell getShell() {
		if (changeButton == null)
			return null;
		return changeButton.getShell();
	}

	public void setChangeButtonText(String text) {
		Assert.isNotNull(text);
		changeButtonText = text;
		if (changeButton != null)
			changeButton.setText(text);
	}

	public void setEnabled(boolean enabled){
		super.setEnabled(enabled); // label
		if (getComboControl()!=null) {
			getComboControl().setEnabled(enabled);
		}
		if (this.changeButton!=null) {
			this.changeButton.setEnabled(enabled);
		}
	}
	
	protected Button getChangeControl() {
		return changeButton;
	}
	
	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), getTextChangeControl(parent)};
	}

	protected String changePressed() {
		if(change == null) return null;
		Properties p = new Properties();
		p.put("shell", changeButton.getShell());
		change.setObject(p);
		int i = change.execute();
		if(i != 0) return null;
		return p.getProperty("value");
	}
}
