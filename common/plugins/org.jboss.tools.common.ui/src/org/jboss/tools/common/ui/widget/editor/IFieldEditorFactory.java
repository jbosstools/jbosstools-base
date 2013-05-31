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
package org.jboss.tools.common.ui.widget.editor;

import java.util.List;

import org.jboss.tools.common.ui.IValidator;
import org.jboss.tools.common.ui.widget.editor.ButtonFieldEditor.ButtonPressedAction;

public interface IFieldEditorFactory {

	final IFieldEditorFactory INSTANCE = new SwtFieldEditorFactory();

	/**
	 * Creates field editor with label and text input control.
	 * 
	 * @param name - logic name of the editor
	 * @param label - text for label control rendered near text input
	 * @param defaultValue - initial value of text control
	 * @return
	 */
	IFieldEditor createTextEditor(String name, String label, String defaultValue);

	/**
	 * Creates field editor with label and text input control.
	 * 
	 * @param name - logic name of the editor
	 * @param label - text for label control rendered near text input
	 * @param defaultValue - initial value of text control
	 * @param description - human friendly explanation of what edited value is for 
	 * @return
	 */
	IFieldEditor createTextEditor(String name, String label, String defaultValue, String description);

	/**
	 * Creates field editor with label and combo input control.
	 * 
	 * @param name - logic name of the editor
	 * @param label - text for label control rendered near combo input
	 * @param values
	 * @param defaultValue - initial value of combo control
	 * @return
	 */
	ITaggedFieldEditor createComboEditor(String name, String label,
			List values, Object defaultValue);

	/**
	 * Creates field editor with label and combo input control.
	 * 
	 * @param name - logic name of the editor
	 * @param label - text for label control rendered near combo input
	 * @param values
	 * @param defaultValue - initial value of text area
	 * @param flat - if true, text area of the combo control is editable
	 * @return
	 */
	ITaggedFieldEditor createComboEditor(String name, String label,
			List values, Object defaultValue, boolean flat);

	/**
	 * Creates field editor with label and combo input control.
	 * 
	 * @param name - logic name of the editor
	 * @param label - text for label control rendered near combo input
	 * @param values
	 * @param defaultValue - initial value of text area
	 * @param flat - if true, text area of the combo control is editable
	 * @param description - human friendly explanation of what edited value is for 
	 * @return
	 */
	ITaggedFieldEditor createComboEditor(String name, String label,
			List values, Object defaultValue, boolean editable, String description);

	/**
	 * Creates field editor with label and set of radio input controls.
	 * 
	 * @param name - logic name of the editor
	 * @param label - text for label control rendered near checkbox input
	 * @param labels - human friendly text presentations of values
	 * @param values - values associated with radio controls
	 * @param defaultValue - initial value of editor, selected radio control
	 * @return
	 */
	ITaggedFieldEditor createRadioEditor(String name, String label,
			List<String> labels, List values, Object defaultValue);

	/**
	 * Creates field editor with label and set of radio input controls.
	 * 
	 * @param name - logic name of the editor
	 * @param label - text for label control rendered near checkbox input
	 * @param labels - human friendly text presentations of values
	 * @param values - values associated with radio controls
	 * @param defaultValue - initial value of editor, selected radio control
	 * @param description - human friendly explanation of what edited value is for 
	 * @return
	 */
	ITaggedFieldEditor createRadioEditor(String name, String label,
			List<String> labels, List values, Object defaultValue, String description);

	/**
	 * Creates field editor with label and checkbox input control.
	 * 
	 * @param name - logic name of the editor
	 * @param label - text for label control rendered near checkbox input
	 * @param defaultValue - initial state of checkbox control
	 * @return
	 */
	IFieldEditor createCheckboxEditor(String name, String label,
			boolean defaultValue);

	/**
	 * Creates field editor with label and checkbox input control.
	 * 
	 * @param name - logic name of the editor
	 * @param label - text for label control rendered near checkbox input
	 * @param defaultValue - initial state of checkbox control
	 * @param description - human friendly explanation of what edited value is for 
	 * @return
	 */
	public IFieldEditor createCheckboxEditor(String name, String label,
			boolean defaultValue, String description);

	/**
	 * 
	 * @param name
	 * @param label
	 * @param defaultValue - initial value of text control
	 * @return
	 */
	IFieldEditor createBrowseFolderEditor(String name, String label, String defaultValue);

	/**
	 * 
	 * @param name
	 * @param label
	 * @param defaultValue
	 * @return
	 */
	IFieldEditor createBrowseWorkspaceFolderEditor(String name, String label, String defaultValue);

	/**
	 * 
	 * @param name
	 * @param label
	 * @param defaultValue
	 * @return
	 */
	IFieldEditor createBrowseSourceFolderEditor(String name, String label, String defaultValue);

	/**
	 * 
	 * @param name
	 * @param label
	 * @param initProjectName
	 * @param defaultValue
	 * @return
	 */
	IFieldEditor createBrowsePackageEditor(String name,	String label, String initSourceFolderPath, String defaultValue);

	/**
	 * 
	 * @param name
	 * @param label
	 * @param defaultValue
	 * @return
	 */
	IFieldEditor createBrowseFileEditor(String name, String label,
			String defaultValue);

	/**
	 * 
	 * @param name
	 * @param label
	 * @param defaultValue
	 * @return
	 */
	IFieldEditor createUneditableTextEditor(String name,
			String label, String defaultValue);
	
	/**
	 * Creates field editor with label, text input control, and button control.
	 * 
	 * @param name - logic name of the editor
	 * @param label - text for label control rendered near text input
	 * @param defaultValue - initial value of text control
	 * @param action - action executed when button is pushed
	 * @param validator
	 * @return
	 */
	IFieldEditor createButtonFieldEditor(String name, String label,
			String defaultValue, ButtonFieldEditor.ButtonPressedAction action,
			IValidator validator);

	/**
	 * Creates field editor with label, text input control, and button control.
	 * 
	 * @param name - logic name of the editor
	 * @param label - text for label control rendered near text input
	 * @param defaultValue - initial value of text control
	 * @param action - action executed when button is pushed
	 * @param validator
	 * @param description - human friendly explanation of what edited value is for 
	 * @return
	 */
	IFieldEditor createButtonFieldEditor(String name, String label, String defaultValue, ButtonFieldEditor.ButtonPressedAction action, IValidator validator, String description);

	/**
	 * 
	 * @param name
	 * @param label
	 * @param defaultValue
	 * @param actions
	 * @param validator
	 * @return
	 */
	public IFieldEditor createButtonFieldEditor(String name, String label,
			String defaultValue, ButtonFieldEditor.ButtonPressedAction[] actions, 
			IValidator validator);

	/**
	 * 
	 * @param name
	 * @param label
	 * @param defaultValue
	 * @param buttonAction
	 * @param linkAction
	 * @param validator
	 * @return
	 */
	public IFieldEditor createButtonAndLinkFieldEditor(String name, String label, String defaultValue, ButtonFieldEditor.ButtonPressedAction buttonAction, ButtonFieldEditor.ButtonPressedAction linkAction, IValidator validator);

	/**
	 * 
	 * @param name
	 * @param label
	 * @param values
	 * @param defaultValue
	 * @param editable
	 * @param action1
	 * @param action2
	 * @param validator
	 * @return
	 */
	IFieldEditor createComboWithTwoButtons(String name, String label,
			List values, Object defaultValue, boolean flat,
			ButtonFieldEditor.ButtonPressedAction action1,
			ButtonFieldEditor.ButtonPressedAction action2, IValidator validator);
	
	ButtonFieldEditor.ButtonPressedAction createNotImplementedYetAction(String buttonName);

	IFieldEditor createComboWithButton(String name, String label,
			List values, Object defaultValue, boolean flat,
			ButtonPressedAction action1,
			IValidator validator);
}
