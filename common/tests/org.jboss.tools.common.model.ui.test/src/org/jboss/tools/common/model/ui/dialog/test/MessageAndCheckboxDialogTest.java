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

import java.util.Properties;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.model.ui.dialog.MessageAndCheckboxDialog;

import junit.framework.TestCase;

/**
 * @author eskimo
 *
 */
public class MessageAndCheckboxDialogTest extends TestCase {

	public void testDialogSingleCheckBox() {
		Properties properties = new Properties();
		properties.put(MessageAndCheckboxDialog.MESSAGE, "Message");
		properties.put(MessageAndCheckboxDialog.CHECKBOX_MESSAGE, "Checkbox message");
		properties.put(MessageAndCheckboxDialog.CHECKED, Boolean.TRUE);
		properties.put(MessageAndCheckboxDialog.BUTTONS, new String[]{" B1","B2"});
		MessageAndCheckboxDialog dialog = new MessageAndCheckboxDialog(Display.getCurrent().getActiveShell(),"title",MessageDialog.ERROR,properties);
		dialog.setBlockOnOpen(false);
		dialog.open();
		dialog.close();
	}	

	public void testDialogMultiCheckBox() {
		Properties properties = new Properties();
		properties.put(MessageAndCheckboxDialog.MESSAGE, "Message");
		properties.put(MessageAndCheckboxDialog.CHECKBOX_MESSAGE, "Checkbox message");
		properties.put(MessageAndCheckboxDialog.CHECKBOX_MESSAGE+"_1", "Checkbox message1");
		properties.put(MessageAndCheckboxDialog.CHECKBOX_MESSAGE+"_2", "Checkbox message2");
		properties.put(MessageAndCheckboxDialog.CHECKED, Boolean.TRUE);
		properties.put(MessageAndCheckboxDialog.CHECKED + "_1", Boolean.TRUE);
		properties.put(MessageAndCheckboxDialog.CHECKED + "_2", Boolean.TRUE);
		properties.put(MessageAndCheckboxDialog.SEPARATOR + "_" + 1, "5");
		properties.put(MessageAndCheckboxDialog.SEPARATOR + "_" + 2, "5");
		MessageAndCheckboxDialog dialog = new MessageAndCheckboxDialog(Display.getCurrent().getActiveShell(),"title",MessageDialog.ERROR,properties);
		dialog.setBlockOnOpen(false);
		dialog.open();
		dialog.close();
	}
	
	public void testDialogMultiCheckBoxNoSeparator() {
		Properties properties = new Properties();
		properties.put(MessageAndCheckboxDialog.MESSAGE, "Message");
		properties.put(MessageAndCheckboxDialog.CHECKBOX_MESSAGE, "Checkbox message");
		properties.put(MessageAndCheckboxDialog.CHECKBOX_MESSAGE+"_1", "Checkbox message1");
		properties.put(MessageAndCheckboxDialog.CHECKBOX_MESSAGE+"_2", "Checkbox message2");
		properties.put(MessageAndCheckboxDialog.CHECKED, Boolean.TRUE);
		properties.put(MessageAndCheckboxDialog.CHECKED + "_1", Boolean.TRUE);
		properties.put(MessageAndCheckboxDialog.CHECKED + "_2", Boolean.TRUE);
		MessageAndCheckboxDialog dialog = new MessageAndCheckboxDialog(Display.getCurrent().getActiveShell(),"title",MessageDialog.ERROR,properties);
		dialog.setBlockOnOpen(false);
		dialog.open();
		dialog.close();
	}

	/**
	 * Test method for {@link org.jboss.tools.common.model.ui.dialog.MessageAndCheckboxDialog#buttonPressed(int)}.
	 */
	public void testButtonPressed() {
		Properties properties = new Properties();
		properties.put(MessageAndCheckboxDialog.MESSAGE, "Message");
		properties.put(MessageAndCheckboxDialog.CHECKBOX_MESSAGE, "Checkbox message");
		properties.put(MessageAndCheckboxDialog.CHECKED, Boolean.TRUE);
		MessageAndCheckboxDialogWrapperTest dialog = new MessageAndCheckboxDialogWrapperTest(Display.getCurrent().getActiveShell(),"title",MessageDialog.ERROR,properties);
		dialog.setBlockOnOpen(false);
		dialog.open();
		dialog.buttonPressed(MessageAndCheckboxDialog.OK);
		assertEquals(dialog.getReturnCode(), Window.OK);
		
		dialog = new MessageAndCheckboxDialogWrapperTest(Display.getCurrent().getActiveShell(),"title",MessageDialog.ERROR,properties);
		dialog.setBlockOnOpen(false);
		dialog.open();
		dialog.buttonPressed(MessageAndCheckboxDialog.CANCEL);
		assertEquals(dialog.getReturnCode(), Window.CANCEL);
	}
	
	
	public static class MessageAndCheckboxDialogWrapperTest extends MessageAndCheckboxDialog {

		public MessageAndCheckboxDialogWrapperTest(Shell parentShell,
				String dialogTitle, int dialogImageType, Properties p) {
			super(parentShell, dialogTitle, dialogImageType, p);
		}
		@Override
		public void buttonPressed(int buttonId) {
			super.buttonPressed(buttonId);
		}
		
	}
}
