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
	 * 
	 * @param name
	 *            TODO
	 * @param label
	 * @param defaultValue
	 * @return
	 */
	IFieldEditor createTextEditor(String name, String label, String defaultValue);

	/**
	 * 
	 * @param name
	 *            TODO
	 * @param label
	 * @param values
	 * @param defaultValue
	 * @return
	 */
	ITaggedFieldEditor createComboEditor(String name, String label,
			List values, Object defaultValue);

	/**
	 * 
	 * @param name
	 *            TODO
	 * @param label
	 * @param values
	 * @param defaultValue
	 * @return
	 */
	ITaggedFieldEditor createComboEditor(String name, String label,
			List values, Object defaultValue, boolean flat);
	
	/**
	 * 
	 * @param name
	 * @param label
	 * @param labels
	 * @param values
	 * @param defaultValue
	 * @return
	 */
	ITaggedFieldEditor createRadioEditor(String name, String label,
			List<String> labels, List values, Object defaultValue);

	/**
	 * 
	 * @param name
	 *            TODO
	 * @param label
	 * @param defaultValue
	 * @return
	 */
	IFieldEditor createCheckboxEditor(String name, String label,
			boolean defaultValue);

	/**
	 * 
	 * @param name
	 * @param label
	 * @param defaultValue
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
	 * 
	 * @param name
	 * @param label
	 * @param defaultValue
	 * @param action
	 * @param validator
	 * @return
	 */
	IFieldEditor createButtonFieldEditor(String name, String label,
			String defaultValue, ButtonFieldEditor.ButtonPressedAction action,
			IValidator validator);

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