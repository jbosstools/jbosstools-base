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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IconAndMessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.reporting.ProblemReportingHelper;
import org.jboss.tools.common.model.ui.reporting.ProblemReporter;
import org.jboss.tools.common.model.ui.reporting.ReportPreference;

public class ErrorDialog extends IconAndMessageDialog {	
	private Control dropDown;
	private Throwable exception;
	private Button detailsButton;
	private Button getAwayButton;
	private String title;
	protected static final int TEXT_HEIGHT = 200;
	private boolean dropDownCreated = false;
	private String problemText;
	
	Text text;

	public ErrorDialog(Shell shell, String title, String message, Throwable exception) {
		super(shell);
		setShellStyle(SWT.DIALOG_TRIM | SWT.RESIZE | SWT.APPLICATION_MODAL);
		this.exception = exception;
		this.title = title;
		this.message = message;
	}
	
	public ErrorDialog(Shell shell, String title, Throwable exception) {
		super(shell);
		setShellStyle(SWT.DIALOG_TRIM | SWT.RESIZE | SWT.APPLICATION_MODAL);
		this.exception = exception;
		this.message = exception.getMessage();
		this.title = title;
	}
	
	protected static boolean shouldDisplay(IStatus status, int mask) {
		return true;
	}
	
	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		shell.setText(title);
	}

	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, "Submit", false);
		if(exception != null) {
			detailsButton =	createButton(parent, IDialogConstants.DETAILS_ID, IDialogConstants.SHOW_DETAILS_LABEL, false);
			createButton(parent, IDialogConstants.CANCEL_ID, "Cancel", false);
		}
	}

	protected Control createDialogArea(Composite parent) {
		createMessageArea(parent);
		
		getAwayButton = new Button(parent, SWT.CHECK);
		getAwayButton.setSelection(false);
		getAwayButton.setText("Do not show this dialog again");
		GridData data = new GridData(GridData.FILL_HORIZONTAL);
		data.horizontalSpan = 2;
		getAwayButton.setLayoutData(data);

		// create a composite with standard margins and spacing
		Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(createLayout());
		GridData childData = new GridData(GridData.FILL_BOTH);
		childData.horizontalSpan = 2;
		composite.setLayoutData(childData);
		composite.setFont(parent.getFont());
		return composite;

	}
	
	GridLayout createLayout() {
		GridLayout gl = new GridLayout(2, false);
		gl.marginHeight = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
		gl.marginWidth = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_MARGIN);
		gl.verticalSpacing = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_SPACING);
		gl.horizontalSpacing = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_SPACING);
		return gl;
	}

	protected Control createDropDownControl(Composite parent) {
		// create the list
		text = new Text(parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.MULTI);

		// print the stacktrace in the text field
		text.setText((problemText==null || problemText.trim().length()==0)?createProblemText():problemText);

		GridData data =	new GridData(
				GridData.HORIZONTAL_ALIGN_FILL
					| GridData.GRAB_HORIZONTAL
					| GridData.VERTICAL_ALIGN_FILL
					| GridData.GRAB_VERTICAL | GridData.FILL_BOTH);
		data.heightHint = TEXT_HEIGHT;
		data.horizontalSpan = 2;
		text.setLayoutData(data);
		text.setFont(parent.getFont());
		
		dropDownCreated = true;

		return text;
	}
	
	private String createProblemText() {
		String problem = exception == null ? "" : ProblemReporter.throwableToString(message, exception);
		return wrapProblem(problem);
	}
	
	private String wrapProblem(String problem) {
		StringBuffer sb = new StringBuffer();
//		sb.append(getEnvironment()).append("\n");
		sb.append("------Type your additional comment here--------\n\n");

		sb.append("-----------------------------------------------\n");
		sb.append(problem);
		return sb.toString();
	}
	
	/**
	 * @return
	 */	
	private String getEnvironment() {
		return CommonPlugin.getEnvironment();
	}
	
	protected void buttonPressed(int id) {
		if(text == null || text.isDisposed()) {
			if(problemText==null) {
				problemText = createProblemText();
			}
		} else {
			problemText = text.getText();
		}
		if (id == IDialogConstants.DETAILS_ID) {
			// was the details button pressed?
			toggleDetailsArea();
		} else {
			super.buttonPressed(id);
		}
	}

    protected void okPressed() {
    	if(getAwayButton.getSelection()) {
    		ReportPreference.SHOW_ERROR_DIALOG_OPTION.setValue("no");
    		ReportPreference.SUBMIT_AUTOMATICALLY_OPTION.setValue("yes");
    	}
    	super.okPressed();
    	submit();
    }
    
    protected void cancelPressed() {
    	if(getAwayButton.getSelection()) {
    		ReportPreference.SHOW_ERROR_DIALOG_OPTION.setValue("no");
    	}
    	super.cancelPressed();
    }

    private void submit() {
    	String email = ReportPreference.E_MAIL_OPTION.getValue();
    	String other = ReportPreference.OTHER_OPTION.getValue();
    	String text = "" + getEnvironment() + "\n" + problemText;
    	ProblemReportingHelper.buffer.report(text, email, other, false);
    }

	private void toggleDetailsArea() {
		Point windowSize = getShell().getSize();
		Point oldSize = getShell().computeSize(SWT.DEFAULT, SWT.DEFAULT);

		if (dropDownCreated) {
			dropDown.dispose();
			dropDownCreated = false;
			detailsButton.setText(IDialogConstants.SHOW_DETAILS_LABEL);
		} else {
			dropDown = createDropDownControl((Composite) getContents());
			detailsButton.setText(IDialogConstants.HIDE_DETAILS_LABEL);
		}

		Point newSize = getShell().computeSize(SWT.DEFAULT, SWT.DEFAULT);

		getShell().setSize(
			new Point(windowSize.x, windowSize.y + (newSize.y - oldSize.y)));

	}

	public static int openError(Shell shell, Throwable exception) {
		ErrorDialog dialog = new ErrorDialog(shell, WizardKeys.getString("ErrorDialog.DefaultWindowTitle"), exception);
		return dialog.open();		
	}

	public static int openError(Shell shell, String title, String message, Throwable t) {
		return new ErrorDialog(shell, title, message, t).open();
	}

	public static int openError(Shell shell, String title, String message) {
		ErrorDialog dialog = new ErrorDialog(shell, title, message, null);
		return dialog.open();		
	}

	public static int openError(Shell shell, String message) {
		ErrorDialog dialog = new ErrorDialog(shell, WizardKeys.getString("ErrorDialog.DefaultWindowTitle"), message, null);
		return dialog.open();		
	}
	
	protected Image getImage() {
		return Display.getDefault().getSystemImage(SWT.ICON_ERROR);
	}

}
