/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.ui.dialog;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * A MessageDialog that displays a check box under the message. By default, the check box is unselected.
 * 
 * @author Xavier Coulon
 *
 */
public class CheckboxMessageDialog extends MessageDialog {

	public static final int CHECKBOX_SELECTED = 2;
	private final String checkboxMessage;

	private boolean checked;
	
	public CheckboxMessageDialog(Shell parentShell, String dialogTitle, String dialogMessage,
			String checkboxMessage) {
		super(parentShell, dialogTitle, null, dialogMessage, CONFIRM, new String[] { IDialogConstants.OK_LABEL,
				IDialogConstants.CANCEL_LABEL }, 1 );
		setShellStyle(getShellStyle() | (SWT.NONE & SWT.SHEET));
		this.checkboxMessage = checkboxMessage;
	}
	
	public CheckboxMessageDialog(Shell parentShell, String dialogTitle, String dialogMessage,
			String checkboxMessage, boolean defaultCheckboxSelection) {
		this(parentShell, dialogTitle, dialogMessage, checkboxMessage);
		this.checked = defaultCheckboxSelection;
	}

	@Override
	public Image getImage() {
		return getQuestionImage();
	}


	@Override
	protected Control createDialogArea(Composite parent) {
		Composite dialogAreaComposite = (Composite) super
                .createDialogArea(parent);
        setToggleButton(createToggleButton(dialogAreaComposite));
        return dialogAreaComposite;
	}
	
	private void setToggleButton(final Button button) {
		button.setText(checkboxMessage);
        button.setSelection(checked);
		
	}

	/**
     * Creates a toggle button without any text or state.  The text and state
     * will be created by <code>createDialogArea</code>. 
     * 
     * @param parent
     *            The composite in which the toggle button should be placed;
     *            must not be <code>null</code>.
     * @return The added toggle button; never <code>null</code>.
     */
    protected Button createToggleButton(Composite parent) {
        final Button button = new Button(parent, SWT.CHECK | SWT.LEFT);

        GridData data = new GridData(SWT.NONE);
        data.horizontalSpan = 2;
        button.setLayoutData(data);
        button.setFont(parent.getFont());

        button.addSelectionListener(new SelectionAdapter() {

			public void widgetSelected(SelectionEvent e) {
				checked = button.getSelection();
            }

        });

        return button;
    }

	@Override
	public int open() {
		int result = super.open();
		if(checked) {
			result += CHECKBOX_SELECTED;
		}
		return result;
	}
	
}
