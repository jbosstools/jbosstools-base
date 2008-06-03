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

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.util.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public abstract class StringButtonFieldEditor extends StringFieldEditor {

	private Button button;
	private String buttonName;

	public StringButtonFieldEditor() {}
	
	public StringButtonFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	protected void adjustForNumColumns(int numColumns) {
		((GridData)getTextControl().getLayoutData()).horizontalSpan = numColumns - 2;
	}

	protected abstract String changePressed();

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		super.doFillIntoGrid(parent, numColumns - 1);
		button = getChangeControl(parent);
		GridData gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.heightHint = convertVerticalDLUsToPixels(button, 14/*IDialogConstants.BUTTON_HEIGHT*/);
		int widthHint = convertHorizontalDLUsToPixels(button, IDialogConstants.BUTTON_WIDTH);
		gd.widthHint = Math.max(widthHint, button.computeSize(SWT.DEFAULT, SWT.DEFAULT, true).x);
		button.setLayoutData(gd);
	}

	protected Button getChangeControl(Composite parent) {
		if (button != null) {
			checkParent(button, parent);
			return button;
		}
		int style = getSettings().getStyle("Button.Style");
		if (style == SWT.DEFAULT) style = SWT.NONE;
		if (style == 0) style = SWT.PUSH;
///		Color bg = getSettings().getColor("Button.Background");
		Color fg = getSettings().getColor("Button.Foreground");
		Font font = getSettings().getFont("Button.Font");
		button = new Button(parent, style);
		button.setFont(font);
///		button.setBackground(bg);
		button.setForeground(fg);
		if (buttonName == null)
			buttonName = JFaceResources.getString("openChange");//$NON-NLS-1$
		button.setText(buttonName);
		button.setFont(parent.getFont());
		addListeners();
		return button;
	}
	void addListeners() {
		button.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent evt) {
				String oldValue = getStringValue();
				String newValue = changePressed();
				if (newValue != null) {
					setStringValue(oldValue);
					setStringValue(newValue);
				}
			}
		});
		button.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent event) {
				button = null;
			}
		});
	}

	public int getNumberOfControls() {
		return 3;
	}

	protected Shell getShell() {
		if (button == null)
			return null;
		return button.getShell();
	}

	public void setChangeButtonText(String text) {
		Assert.isNotNull(text);
		buttonName = text;
		if (button != null)
			button.setText(text);
	}

	public void setEnabled(boolean enabled){
		super.setEnabled(enabled); // label
		if (getTextControl()!=null) {
			boolean b = isAlwaysReadOnly();
			getTextControl().setEnabled(enabled || b);
		}
		if (this.button!=null) {
			this.button.setEnabled(enabled);
		}
	}
	
	protected Button getChangeControl() {
		return button;
	}
	
}
