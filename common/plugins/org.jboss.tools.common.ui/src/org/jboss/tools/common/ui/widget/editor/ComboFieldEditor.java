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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.ui.widget.field.ComboBoxField;

public class ComboFieldEditor extends BaseFieldEditor implements ITaggedFieldEditor,PropertyChangeListener{

	List values = null;

	boolean editable = false;

	public ComboFieldEditor(String name, String label, List values,Object defaultValue,boolean editableSelection) {
		super(name, label, defaultValue);
		this.values = Collections.unmodifiableList(values);
		this.editable = editableSelection;
	}

	private ComboBoxField comboField;

	@Override
	public Object[] getEditorControls(Object composite) {
		return new Control[] {getComboControl((Composite)composite)};
	}

	@Override
	public void doFillIntoGrid(Object parent) {
	}

	public Control getComboControl(Composite composite) {
		if(comboField == null) {
			comboField = new ComboBoxField(composite,values,getValue(),editable);
			comboField.addPropertyChangeListener(this);
			final Combo combo =comboField.getComboControl();
			combo.addDisposeListener(new DisposeListener(){
				public void widgetDisposed(DisposeEvent e) {
					dispose(e);
					combo.removeDisposeListener(this);
				}
			});
		} else if(composite!=null) {
			Assert.isTrue(comboField.getControl().getParent()==composite);
		}
		return comboField.getControl();
	}

	@Override
	public Object[] getEditorControls() {
		return new Control[]{comboField.getControl()};
	}

	public void save(Object object) {
	}

	public void propertyChange(PropertyChangeEvent evt) {
		setValue(evt.getNewValue());
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.ITaggedFieldEditor#getTags()
	 */
	public String[] getTags() {
		return comboField.getComboControl().getItems();
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.ITaggedFieldEditor#setTags(java.lang.String[])
	 */
	public void setTags(String[] tags) {
		if(comboField!=null) {
			comboField.setTags(tags,getValueAsString());
		}
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.BaseFieldEditor#getNumberOfControls()
	 */
	@Override
	public int getNumberOfControls() {
		return 1;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.CompositeEditor#setEditable(boolean)
	 */
	@Override
	public void setEditable(boolean ediatble) {
		super.setEditable(ediatble);
		comboField.setEditable(ediatble);
	}

	public void setValue(Object newValue) {
		if(newValue==null) {
			return;
		}
		super.setValue(newValue);
		if(comboField!=null) {
			comboField.removePropertyChangeListener(this);
			comboField.setValue(newValue.toString());
			comboField.addPropertyChangeListener(this);
		}
	}
}