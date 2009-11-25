/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.ui.widget.editor;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.ui.CommonUIMessages;
import org.jboss.tools.common.ui.widget.field.PushButtonField;

/**
 * @author eskimo
 *
 */
public class ButtonFieldEditor extends BaseFieldEditor {

	PushButtonField button= null;
	
	private ButtonPressedAction buttonAction = new ButtonPressedAction(CommonUIMessages.BUTTON_FIELD_EDITOR_BROWSE) {
		@Override
		public void run() {
			throw new IllegalStateException(CommonUIMessages.BUTTON_FIELD_EDITOR_NOT_IMPLEMENTED_YET);
		}
	};
	
	public ButtonFieldEditor(String name, String label) {
		super(name, label, new Object());
	}
	
	public ButtonFieldEditor(String name, ButtonPressedAction action, Object defaultValue) {
		super(name, action.getText(), defaultValue);
		buttonAction = action;
		buttonAction.setFieldEditor(this);
	}

	@Override
	public void doFillIntoGrid(Object parent) {
	}

	@Override
	public Object[] getEditorControls() {
		if(button==null) {
			return null;
		}
		return new Control[]{button.getControl()};
	}

	@Override
	public boolean isEditable() {
		return false;
	}

	public void save(Object object) {
	}

	@Override
	public void setEditable(boolean ediatble) {
	}

	@Override
	public Object[] getEditorControls(Object composite) {
		if(button==null && composite!=null) {
			button = new PushButtonField((Composite)composite,buttonAction);
			setEnabled(isEnabled());
		}
		return new Control[]{button.getControl()};
	}

	public ButtonPressedAction getButtonaction() {
		return buttonAction;
	}

	public static class ButtonPressedAction extends Action implements SelectionListener{

		private IFieldEditor editor = null;

		public ButtonPressedAction(String label) {
			super(label);
		}
		
		public void setFieldEditor(IFieldEditor newEditor) {
			editor = newEditor;
		}
		
		public IFieldEditor getFieldEditor() {
			return editor;
		}

		public void widgetDefaultSelected(SelectionEvent e) {
		}
		
		public void widgetSelected(SelectionEvent e) {
				run();
		}
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.BaseFieldEditor#getNumberOfControls()
	 */
	@Override
	public int getNumberOfControls() {
		return 1;
	}
}