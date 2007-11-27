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

import org.eclipse.core.runtime.IAdaptable;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Composite;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public abstract class ValueEditor extends PropertyEditor { // {implements ICellEditorListener {

	public ValueEditor() {}

	public ValueEditor(IWidgetSettings settings) {
		super(settings);
	}

	protected abstract CellEditor createCellEditor(Composite parent);
	protected abstract ExtendedFieldEditor createFieldEditor(Composite parent);

	protected CellEditor cellEditor;
	protected ExtendedFieldEditor fieldEditor;
//	private IValueProvider valueProvider;
//	private IValueChangeListener valueChangeListener;
	
	public void dispose() {
		super.dispose();
		if (cellEditor!=null) cellEditor.dispose();
		cellEditor = null;
		if (fieldEditor!=null) fieldEditor.dispose();
		fieldEditor = null;
	}
	
	public CellEditor getCellEditor(Composite parent) {
		if (cellEditor==null) {
			cellEditor = createCellEditor(parent);
			if (cellEditor!=null) {
				IValueProvider valueProvider = (IValueProvider)getAdapter(IValueProvider.class);
//				IValueChangeListener valueChangeListener = (IValueChangeListener)getAdapter(IValueChangeListener.class);
				cellEditor.setValue(valueProvider.getValue());
				//cellEditor.addListener(this);
			}
		}
		return cellEditor;
	}
	public ExtendedFieldEditor getFieldEditor(Composite parent) {
		if (fieldEditor==null) {
			fieldEditor = createFieldEditor(parent);
			fieldEditor.setLabelText(getLabelText());
			if (fieldEditor instanceof IPropertyFieldEditor) {
				((IPropertyFieldEditor)fieldEditor).setPropertyEditor(this);
			}
			//bug fix 7894
			parent.addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent e) {
					fieldEditor = null;
				}
			});
		}
		return fieldEditor;
	}

	public Object getInput() {
		if (input == null) {
			setInput(new DefaultValueAdapter());
		}
		return input;
	}
	
	// getValue() and setValue() for Slava

	public Object getValue() {
		return ((IValueProvider)((IAdaptable)getInput()).getAdapter(IValueProvider.class)).getValue();
	}
	public void setValue(Object value){
		PropertyChangeEvent event = new PropertyChangeEvent(this, IPropertyEditor.VALUE, getValue(), value);
		((IValueChangeListener)((IAdaptable)getInput()).getAdapter(IValueChangeListener.class)).valueChange(event);
	}
	
}
