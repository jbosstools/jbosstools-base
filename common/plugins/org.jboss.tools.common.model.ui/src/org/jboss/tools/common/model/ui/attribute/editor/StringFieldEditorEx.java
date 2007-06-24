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

import org.jboss.tools.common.model.ui.IAttributeErrorProvider;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class StringFieldEditorEx extends StringFieldEditor implements IFieldEditor, IPropertyFieldEditor, IPropertyChangeListener, PropertyChangeListener {
	
	protected IPropertyEditor propertyEditor;
	
	protected IValueProvider valueProvider;
	protected IValueChangeListener valueChangeListener;

	public StringFieldEditorEx() {}
	
	public StringFieldEditorEx(IWidgetSettings settings) {
		super(settings);
	}

	public StringFieldEditorEx(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
	}

	protected void init() {
		setStringValue(valueProvider.getStringValue(true));
		setPropertyChangeListener(this);
		valueProvider.addValueChangeListener(this);
		
	}

	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
			setErrorProvider((IAttributeErrorProvider)propertyEditor.getAdapter(IAttributeErrorProvider.class));
		}
		init();
	}

	// IPropertyChangeListener
	public void propertyChange(org.eclipse.jface.util.PropertyChangeEvent event) {
		if(ExtendedFieldEditor.VALUE.equals(event.getProperty())) {
			setPropertyChangeListener(null);
			PropertyChangeEvent e = new PropertyChangeEvent(this, IPropertyEditor.VALUE, event.getOldValue(), event.getNewValue());
			valueChangeListener.valueChange(e);
			setPropertyChangeListener(this);
		}
	}
	
	// PropertyChangeListener
	public void propertyChange(PropertyChangeEvent evt) {
		valueProvider.removeValueChangeListener(this);
		if (IPropertyEditor.VALUE.equals(evt.getPropertyName())) {
			Object v = evt.getNewValue();
			this.setStringValue((v == null) ? "" : v.toString());
		}
		valueProvider.addValueChangeListener(this);
	}
	
	public Control createTextControl(Composite parent) {
		Control control = super.createTextControl(parent);
		Text text = getTextField();
		if (valueProvider!=null) {
			String value = valueProvider.getStringValue(true);
			if (!value.equals(text.getText())) {
				text.setText(value.toString());
			}
		}
		return control;
	}

	// IFieldEditor
	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), createTextControl(parent)};
	}

	protected boolean isAlwaysReadOnly() {
		if(propertyEditor == null) return false;
		Object input = propertyEditor.getInput();
		if(input instanceof DefaultValueAdapter) {
			DefaultValueAdapter a = (DefaultValueAdapter)input;
			XModelObject o = a.getModelObject();
			if(o == null || o.isObjectEditable()) return false;
			while(o != null && o.getFileType() < XModelObject.FOLDER) {
				o = o.getParent();
			}
			if(o == null) return false;
			String entity = o.getModelEntity().getName();
			if(entity.indexOf("Jar") >= 0) return true;
		}
		return false;
	}

	public void setStringValue(String value) {
		if(!isSameValue(value)) {
			super.setStringValue(value);
		}
	}

	boolean isSameValue(String newValue) {
		Text text = getTextField();
		if(text == null || text.isDisposed() || newValue == null) return false;
		String oldTextValue = text.getText();
		if(propertyEditor != null && propertyEditor.getInput() instanceof DefaultValueAdapter) {
			DefaultValueAdapter a = (DefaultValueAdapter)propertyEditor.getInput();
			if(a.getAttribute().isTrimmable()) {
				return oldTextValue != null && oldTextValue.trim().equals(newValue.trim());
			}
		}
		return oldTextValue != null && oldTextValue.equals(newValue);
	}

}
