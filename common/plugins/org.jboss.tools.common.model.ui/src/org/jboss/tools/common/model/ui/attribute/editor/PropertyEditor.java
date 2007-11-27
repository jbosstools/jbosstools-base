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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;

import org.eclipse.core.runtime.IAdaptable;
import org.jboss.tools.common.model.ui.attribute.adapter.IModelPropertyEditorAdapter;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellEditorListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public abstract class PropertyEditor implements IPropertyEditor, PropertyChangeListener {
	
	protected boolean lineEditor = false;
	protected String labelText;
	protected ICellEditorListener cellEditorListener;
	protected IPropertyChangeListener propertyChangeListener;
	protected IWidgetSettings settings;
	
	public PropertyEditor() {}
	
	public PropertyEditor(IWidgetSettings settings) {
		this.settings = settings;
	}

	// creatios for cell editor and field  editor
	protected abstract CellEditor createCellEditor(Composite parent);
	protected abstract ExtendedFieldEditor createFieldEditor(Composite parent);
	
	public boolean isLineEditor() {
		return lineEditor;
	}
	
	public boolean isGreedyEditor() {
		return false;
	}
	
	public boolean callsExternal() {
		return false;
	}

	public Object callExternal(Shell shell) {
		return getValue();
	}

	// getters for cell editor and field  editor
	public CellEditor getCellEditor(Composite parent) {
		return createCellEditor(parent); 
	}
	public ExtendedFieldEditor getFieldEditor(Composite parent) {
		ExtendedFieldEditor fieldEditor = createFieldEditor(parent);
		fieldEditor.setLabelText(getLabelText());
		return fieldEditor;
	}
	
	// getter and setter for label string
	public void setLabelText(String labelText) {
		if (labelText!=null) {
			this.labelText = labelText;
		}
	}
	public String getLabelText() {
		return labelText;
	}

	// listeners
	private ArrayList<IPropertyEditorListener> listeners = new ArrayList<IPropertyEditorListener>();
	public void addPropertyEditorListener(IPropertyEditorListener l) {
		listeners.add(l);
	}
	public void removePropertyEditorListener(IPropertyEditorListener l) {
		listeners.remove(l);
	}
	public void fireInputChanged() {
		ArrayList list = (ArrayList)listeners.clone();
		for (int i=0;i<list.size();++i){
			((IPropertyEditorListener)list.get(i)).inputChangedEvent(new PropertyEditorEvent(this));
		}
		list.clear();
		list = null;
	} 
	public void fireValueChanged() {
		ArrayList list = (ArrayList)listeners.clone();
		for (int i=0;i<list.size();++i){
			((IPropertyEditorListener)list.get(i)).valueChangedEvent(new PropertyEditorEvent(this));
		}
		list.clear();
		list = null;
	} 

	// value
	// getValue() and setValue() for Slava
	public abstract Object getValue();
	public abstract void setValue(Object value);

	// input
	protected Object input;
	public Object getInput() {
		return input;
	}
	public void setInput(Object input) {
		this.input = input;
		fireInputChanged();
	}

	// IAdaptable
	public Object getAdapter(Class adapter) {
		Object object;
		object = ((IAdaptable)getInput()).getAdapter(adapter);
		return object;
	}
	
	public String getAttributeName() {
		if (input instanceof IModelPropertyEditorAdapter)
			if(((IModelPropertyEditorAdapter)input).getAttribute() != null) 
			return ((IModelPropertyEditorAdapter)input).getAttribute().getName();
			
		return labelText;
	}

	public void propertyChange(PropertyChangeEvent evt)
	{
		setValue(evt.getNewValue());
	}

	/**
	 * @return
	 */
	public IWidgetSettings getSettings() {
		return settings;
	}

	/**
	 * @param settings
	 */
	public void setSettings(IWidgetSettings settings) {
		this.settings = settings;
	}

	public String getChangeButtonName() {
		return null;
	}
	
	public void dispose() {
		cellEditorListener = null;
		propertyChangeListener = null;
		settings = null;
	}
}
