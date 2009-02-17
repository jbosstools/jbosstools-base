/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.ui.dialog.test;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.model.ui.dialog.ErrorDialog;
import org.jboss.tools.test.util.JobUtils;

import junit.framework.TestCase;

/**
 * @author eskimo
 *
 */
public class ErrorDialogTest extends TestCase {

	/**
	 * @param name
	 */
	public ErrorDialogTest(String name) {
		super(name);
	}

	/**
	 * Test method for {@link org.jboss.tools.common.model.ui.dialog.ErrorDialog#buttonPressed(int)}.
	 */
	public void testButtonPressed() {
		ErrorDialogTestWrapper errDialog = new ErrorDialogTestWrapper(Display.getCurrent().getActiveShell(),"Title1",new Exception("Exception Message"));
		
		errDialog.setBlockOnOpen(false);
		errDialog.open();
		errDialog.buttonPressed(IDialogConstants.DETAILS_ID);
		errDialog.buttonPressed(IDialogConstants.DETAILS_ID);
		errDialog.close();
	}
	/**
	 * Test method for {@link org.jboss.tools.common.model.ui.dialog.ErrorDialog#cancelPressed()}.
	 */
	public void testCancelPressed() {
		ErrorDialogTestWrapper errDialog = new ErrorDialogTestWrapper(Display.getCurrent().getActiveShell(),"Title1",new Exception("Exception Message"));
		
		errDialog.setBlockOnOpen(false);
		errDialog.open();
		errDialog.buttonPressed(IDialogConstants.CANCEL_ID);
		assertEquals(Window.CANCEL, errDialog.getReturnCode());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.model.ui.dialog.ErrorDialog#okPressed()}.
	 */
	public void testOkPressed() {
		ErrorDialogTestWrapper errDialog = new ErrorDialogTestWrapper(Display.getCurrent().getActiveShell(),"Title1",new Exception("Exception Message"));
		
		errDialog.setBlockOnOpen(false);
		errDialog.open();
		errDialog.buttonPressed(IDialogConstants.OK_ID);
		assertEquals(Window.OK, errDialog.getReturnCode());
	}

	/**
	 * Test method for {@link org.jboss.tools.common.model.ui.dialog.ErrorDialog#ErrorDialog(org.eclipse.swt.widgets.Shell, java.lang.String, java.lang.String, java.lang.Throwable)}.
	 */
	public void testErrorDialogShellStringStringThrowable() {
		ErrorDialogTestWrapper errDialog = new ErrorDialogTestWrapper(Display.getCurrent().getActiveShell(),"Title1","Message",new Exception("Exception Message"));
		
		errDialog.setBlockOnOpen(false);
		errDialog.open();
		errDialog.buttonPressed(IDialogConstants.DETAILS_ID);
		errDialog.buttonPressed(IDialogConstants.DETAILS_ID);
		errDialog.close();
	}

	/**
	 * Test method for {@link org.jboss.tools.common.model.ui.dialog.ErrorDialog#ErrorDialog(org.eclipse.swt.widgets.Shell, java.lang.String, java.lang.String, java.lang.Throwable)}.
	 */
	public void testErrorDialogShellStringThrowable() {
		ErrorDialogTestWrapper errDialog = new ErrorDialogTestWrapper(Display.getCurrent().getActiveShell(),"Title1",new Exception("Exception Message"));
		
		errDialog.setBlockOnOpen(false);
		errDialog.open();
		errDialog.buttonPressed(IDialogConstants.DETAILS_ID);
		errDialog.buttonPressed(IDialogConstants.DETAILS_ID);
		errDialog.close();
	}	
	public static class ErrorDialogTestWrapper extends ErrorDialog {

		public ErrorDialogTestWrapper(Shell shell, String title,
				String message, Throwable exception) {
			super(shell, title, message, exception);
		}

		public ErrorDialogTestWrapper(Shell shell, String title,
				Throwable exception) {
			super(shell, title, exception);
		}

		public void buttonPressed(int id) {
			// TODO Auto-generated method stub
			super.buttonPressed(id);
		}
		
		
	}
}
