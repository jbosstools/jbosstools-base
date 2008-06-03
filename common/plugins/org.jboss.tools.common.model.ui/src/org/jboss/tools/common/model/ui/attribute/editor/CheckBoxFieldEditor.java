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

import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class CheckBoxFieldEditor extends ExtendedFieldEditor implements IFieldEditor, IPropertyFieldEditor, IPropertyChangeListener, PropertyChangeListener {
	
	public static final int DEFAULT= 0;
	public static final int SEPARATE_LABEL= 1;
	public static final int SKIP_LABEL= 2;
	
	private int style = SKIP_LABEL;
	private Button checkBox = null;
	
	protected String trueValue = "true";
	protected String falseValue = "false";
	
	protected boolean booleanValue = false;

	protected IPropertyEditor propertyEditor;
	protected IValueProvider valueProvider;
	protected IValueChangeListener valueChangeListener;

	public CheckBoxFieldEditor() {}

	public CheckBoxFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	protected void adjustForNumColumns(int numColumns) {
		if (style == SEPARATE_LABEL) numColumns--;
		if (style == SKIP_LABEL) numColumns--;
		((GridData)checkBox.getLayoutData()).horizontalSpan = numColumns;
	}
	protected void doFillIntoGrid(Composite parent, int numColumns) {
		String text = getLabelText();
		if (style == SKIP_LABEL) {
			Label label = new Label(parent,SWT.NONE);
			label.setText("");
			numColumns--;
		}
		if (style == SEPARATE_LABEL) {
			getLabelComposite(parent);
			numColumns--;
			text = null;
		}
		checkBox = createChangeControl(parent);
		GridData gd = new GridData();
		gd.horizontalSpan = numColumns;
		checkBox.setLayoutData(gd);
		if (text != null) checkBox.setText(text);
	}
	protected Control createCheckBoxLabelControl (Composite parent) {
		Button checkBox = createChangeControl(parent);
		if (getLabelText() != null) checkBox.setText(getLabelText());
		return checkBox;
	}
	protected void doLoad() {
		throw new RuntimeException("Not implemented");
	}
	protected void doLoadDefault() {
		throw new RuntimeException("Not implemented");
	}
	protected void doStore() {
//		getPreferenceStore().setValue(getPreferenceName(), checkBox.getSelection());
	}
	protected Button createChangeControl(Composite parent) {
		if (checkBox != null) {
			checkParent(checkBox, parent);
			return checkBox;
		}
		int style = 0;
		if (getSettings() != null) {
			style = getSettings().getStyle("CheckBox.Style");
		}
		if (style == SWT.DEFAULT) {
			style = SWT.CHECK | SWT.LEFT;
		}
		checkBox = new Button(parent, style);
		checkBox.setFont(parent.getFont());
		createSelectionListener();
		createDisposeListener();
		checkBox.setSelection(booleanValue);
		return checkBox;
	}
	private void createSelectionListener() {
		checkBox.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				boolean isSelected = checkBox.getSelection();
				valueChanged(booleanValue, isSelected);
				booleanValue = isSelected;
			}
		});
	}
	private void createDisposeListener() {
		checkBox.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent event) {
				checkBox = null;
			}
		});
	}
	public int getNumberOfControls() {
		return (style == SKIP_LABEL || style == SEPARATE_LABEL) ? 2 : 1;
	}
	public void setFocus() {
		if (checkBox != null) {
			checkBox.setFocus();
		}
	}
	public void setLabelText(String text) {
		super.setLabelText(text);
		Control label = getLabelComposite();
		if (label == null && checkBox != null) {
			checkBox.setText(text);
		}
	}
	protected void valueChanged(boolean oldValue, boolean newValue) {
		setPresentsDefaultValue(false);
		if (oldValue != newValue)
			fireStateChanged(VALUE, oldValue, newValue);
	}
	
	public Control getChangeControl() {
		return checkBox;
	}

	public void setEnabled(boolean enabled){
		//Only call super if there is a label already
		if(style == SEPARATE_LABEL)
			super.setEnabled(enabled);
		if (this.getChangeControl()!=null) {
			getChangeControl().setEnabled(enabled);
		}
	}
	
	public void setBooleanValue(boolean isChecked) {
		booleanValue = isChecked;
		if (checkBox != null) {
			boolean oldValue = checkBox.getSelection();
			if (oldValue!=booleanValue) {
				checkBox.setSelection(booleanValue);
				valueChanged(oldValue, booleanValue);
			}
		}
	}
	public boolean getBooleanValue() {
		if (checkBox != null)
			return checkBox.getSelection();
		else
			return getPreferenceStore().getBoolean(getPreferenceName());
	}

	public int getStyle() {
		return style;
	}
	public void setStyle(int style) {
		this.style = style;
	}

	private void initValue(Object value) {
		if (value instanceof String) {
			if ("yes".equalsIgnoreCase(value.toString()) || "no".equalsIgnoreCase(value.toString())) {
				trueValue = "yes";
				falseValue = "no";
			}
		}
	}
	
	private boolean isTrue(Object value) {
		if (value instanceof Boolean) return ((Boolean)value).booleanValue();
		if (value instanceof String) {
			if ("yes".equalsIgnoreCase(value.toString()) || "true".equalsIgnoreCase(value.toString())) return true;
		}
		return false;
	}

	// IPropertyChangeListener
	public void propertyChange(org.eclipse.jface.util.PropertyChangeEvent e) {
		if(ExtendedFieldEditor.VALUE.equals(e.getProperty())) {
			setPropertyChangeListener(null);
			boolean newBooleanValue = ((Boolean)e.getNewValue()).booleanValue();
			boolean oldBooleanValue = ((Boolean)e.getOldValue()).booleanValue();
			String oldValue = (oldBooleanValue)?trueValue:falseValue;
			String newValue = (newBooleanValue)?trueValue:falseValue;
			PropertyChangeEvent event = new PropertyChangeEvent(this, VALUE, oldValue, newValue);
			valueChangeListener.valueChange(event);
			setPropertyChangeListener(this);
		}
	}

	protected void init() {
		initValue(valueProvider.getValue());
		setBooleanValue(isTrue(valueProvider.getValue()));
		setPropertyChangeListener(this);
		valueProvider.addValueChangeListener(this);
	}

	public void dispose() {
		super.dispose();
		if (checkBox!=null && !checkBox.isDisposed()) checkBox.dispose();
		if (valueProvider!=null) valueProvider.removeValueChangeListener(this);
		checkBox = null;
		propertyEditor = null;
		valueProvider = null;
		valueChangeListener = null;
	}

	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
		}
		init();
	}

	// IFieldEditor
	public Control[] getControls(Composite parent) {
		return new Control[] {new Label(parent,SWT.NONE), createCheckBoxLabelControl(parent)};
	}

	// PropertyChangeListener
	public void propertyChange(PropertyChangeEvent evt) {
		valueProvider.removeValueChangeListener(this);
		setBooleanValue(isTrue(evt.getNewValue()));
		valueProvider.addValueChangeListener(this);
	}
	
	public void setIndent(int width) {
		if(checkBox == null || checkBox.isDisposed()) return;
		GridData gd = (GridData)checkBox.getLayoutData();
		gd.horizontalIndent = width;
		checkBox.getParent().update();
	}

	public void cut() {
	}

	public void copy() {
	}

	public void paste() {
	}

	public void delete() {
	}
}
