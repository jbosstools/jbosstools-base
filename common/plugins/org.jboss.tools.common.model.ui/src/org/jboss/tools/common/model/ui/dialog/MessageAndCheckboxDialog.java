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
package org.jboss.tools.common.model.ui.dialog;

import java.util.*;
import org.eclipse.swt.*;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;
import org.eclipse.jface.dialogs.*;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.util.Assert;

import org.jboss.tools.common.model.ServiceDialog;

public class MessageAndCheckboxDialog extends MessageDialog {
	public static String MESSAGE = ServiceDialog.DIALOG_MESSAGE;
	public static String CHECKBOX_MESSAGE = ServiceDialog.CHECKBOX_MESSAGE;
	public static String CHECKED = ServiceDialog.CHECKED;
	public static String BUTTONS = ServiceDialog.BUTTONS;
	public static String SEPARATOR = ServiceDialog.SEPARATOR;
	public static String TITLE = "title";
	public static String RETURN_CODE = ServiceDialog.RETURN_CODE;
	Button button;
	Button[] buttons = new Button[0];
	Properties properties;
	int lastButton = -1;
	
	public MessageAndCheckboxDialog(Shell parentShell, String dialogTitle, int dialogImageType, Properties p) {
		super(parentShell, dialogTitle, null, p.getProperty(MESSAGE), dialogImageType, createButtonLabels(p), 0);
		properties = p;
	}
	
	static String[] createButtonLabels(Properties p) {
		String[] buttons = (String[])p.get(BUTTONS);
		if(buttons == null) return new String[]{IDialogConstants.OK_LABEL, IDialogConstants.CANCEL_LABEL};
		String[] bs = new String[buttons.length];
		for (int i = 0; i < bs.length; i++) {
			bs[i] = JFaceResources.getString(buttons[i]);
			if(bs[i] == null) bs[i] = buttons[i];
		}
		return bs;
	}

	public static boolean openConfirm(Shell parent, Properties p) {
		Assert.isNotNull(p);
		Assert.isNotNull(p.getProperty(MESSAGE), "Property " + MESSAGE + " is null");
		Assert.isNotNull(p.getProperty(CHECKBOX_MESSAGE), "Property " + CHECKBOX_MESSAGE + " is null");
		Assert.isTrue(p.get(CHECKED) instanceof Boolean, "Property " + CHECKED + " must have type Boolean");
		String title = p.getProperty("title", "Confirmation");
		MessageAndCheckboxDialog dialog = new MessageAndCheckboxDialog(parent, title, QUESTION, p);
		int result = dialog.open();
		p.put(RETURN_CODE, new Integer(result));
		return result == 0;
	}
	
	protected Control createCustomArea(Composite parent) {
		String b1 = properties.getProperty(CHECKBOX_MESSAGE + "_1");
		if(b1 == null) return button = createCheckBox(parent, "");
		return createMultiCheckBox(parent);
	}
	
	protected Button createCheckBox(Composite parent, String suffix) {
		Button b = new Button(parent, SWT.CHECK);
		GridData data = new GridData(GridData.FILL_HORIZONTAL);
		if(getImage() != null)
			data.horizontalIndent = getImage().getImageData().width + 8;
		b.setLayoutData(data);
		b.setText(properties.getProperty(CHECKBOX_MESSAGE + suffix));
		b.setSelection(((Boolean)properties.get(CHECKED + suffix)).booleanValue());		
		return b;
	}

	protected Control createMultiCheckBox(Composite parent) {
		Composite c = new Composite(parent, SWT.NONE);
		c.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		GridLayout l = new GridLayout();
		l.marginWidth = 0;
		l.marginHeight = 11;
		c.setLayout(l);
		button = createCheckBox(c, "");
		ArrayList<Button> list = new ArrayList<Button>();
		int i = 1;
		while(true) {
			String suffix = "_" + i;
			if(properties.getProperty(CHECKBOX_MESSAGE + suffix) == null) break;
			if(properties.getProperty(SEPARATOR + suffix) != null) {
				// actually here we should only call
				// createSeparator(parent, true);
				// breaking would be performed 
				// on another property LAST + suffix
				// it is not implemented while we have 
				// the only case for this dialog.
				lastButton = i;
				break;
			}
			Button b = createCheckBox(c, suffix);
			list.add(b);
			++i;
		}
		buttons = list.toArray(new Button[0]);
		return c;
	}
	
	private void createSeparator(Composite parent, boolean indent) {
		Label lb = new Label(parent, SWT.SEPARATOR | SWT.HORIZONTAL);
		GridData data = new GridData(GridData.FILL_HORIZONTAL);
		if(indent && getImage() != null)
			data.horizontalIndent = getImage().getImageData().width + 8;
		lb.setLayoutData(data);
	}
	
	private void createSpace(Composite parent, int height) {
		Label lb = new Label(parent, SWT.HORIZONTAL);
		GridData data = new GridData(GridData.FILL_HORIZONTAL);
		data.heightHint = height;
		lb.setLayoutData(data);
	}
	
	protected void buttonPressed(int buttonId) {
		properties.put(CHECKED, new Boolean(button.getSelection()));
		for (int i = 0; i < buttons.length; i++) {
			properties.put(CHECKED + "_" + (i + 1), new Boolean(buttons[i].getSelection()));
		}
		super.buttonPressed(buttonId);
	}
	
	protected Control createButtonBar(Composite parent) {
		if(lastButton < 0) return super.createButtonBar(parent);
		Composite vcomposite = new Composite(parent, SWT.NONE);
		GridLayout vl = new GridLayout(1, false);
		vl.marginHeight = 0;
		vl.marginWidth = 0;
		vl.verticalSpacing = 0;
		vcomposite.setLayout(vl);
		GridData d = new GridData(GridData.FILL_HORIZONTAL);
		d.horizontalSpan = 2;
		vcomposite.setLayoutData(d);
///		createSpace(vcomposite, 6);
		createSeparator(vcomposite, false);
		createSpace(vcomposite, 10);
		Composite composite = new Composite(vcomposite, SWT.NONE);
		GridLayout l = new GridLayout(2, false);
		l.marginHeight = 0;
		l.marginWidth = 0;
		l.verticalSpacing = 0;
		composite.setLayout(l);
		GridData data = new GridData(GridData.VERTICAL_ALIGN_CENTER | GridData.FILL_HORIZONTAL);
		composite.setLayoutData(data);
		Button b = createCheckBox(composite, "_" + lastButton);
		b.setLayoutData(new GridData());
		Button[] bs = new Button[buttons.length + 1];
		System.arraycopy(buttons, 0, bs, 0, buttons.length);
		bs[buttons.length] = b;
		buttons = bs;
		Composite w = new Composite(composite, SWT.NONE); 
		GridLayout wl = new GridLayout(2, false);
		wl.marginHeight = 0;
		wl.marginWidth = 0;
		wl.horizontalSpacing = 0;
		wl.verticalSpacing = 0;
		w.setLayout(wl);
		w.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		Control c = super.createButtonBar(w);
		c.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END | GridData.FILL_HORIZONTAL));
		return vcomposite;
	}

}
