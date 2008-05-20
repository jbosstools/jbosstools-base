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

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.widgets.Composite;

public interface IPropertyEditor extends IAdaptable {
	public static final String VALUE = "IPropertyEditor.value";
	public static final String LIST_CONTENT = "IPropertyEditor.listContent";
	
	// get components
	public CellEditor getCellEditor(Composite parent);
	public ExtendedFieldEditor getFieldEditor(Composite parent);
	
	// listeners
	public void addPropertyEditorListener(IPropertyEditorListener l);
	public void removePropertyEditorListener(IPropertyEditorListener l);

	// for slava support
	public Object getValue();
	public void setValue(Object value);

	// input object must be implemetation IAdaptable
	public Object getInput();
	public void setInput(Object input);

	// label text
	public void setLabelText(String labelText);
	public String getLabelText();
	
	/*
	 * Returns system name for edited model attribute.
	 */
	public String getAttributeName();
	
	/*
	 * Returns true if field editor requires ample vertical space. 
	 * If form cannot provide required vertical space it creates 
	 * text field and change button that will call dialog with 
	 * the field editor.
	 */	
	public boolean isGreedyEditor();
	
	/*
	 * Returns Name for change button that form creates if it cannot 
	 * provide required vertical space for the field editor
	 */	
	public String getChangeButtonName();
	
	// destroy object
	public void dispose();
}
