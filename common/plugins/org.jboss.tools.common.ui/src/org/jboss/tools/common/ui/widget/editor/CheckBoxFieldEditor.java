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

import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.ui.widget.field.CheckBoxField;

/**
 * @author eskimo
 *
 */
public class CheckBoxFieldEditor extends BaseFieldEditor implements PropertyChangeListener {

	private Control checkBoxControl;

	/**
	 * @param name
	 * @param label
	 * @param defaultValue
	 */
	public CheckBoxFieldEditor(String name, String label, Object defaultValue) {
		super(name, label, defaultValue);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.internal.project.facet.BaseFieldEditor#createEditorControls(java.lang.Object)
	 */
	@Override
	public Object[] getEditorControls(Object composite) {
		return new Control[] {createCheckBoxControl((Composite)composite)};
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.internal.project.facet.BaseFieldEditor#getEditorControls()
	 */
	@Override
	public Object[] getEditorControls() {
		return new Control[] {getCheckBoxControl()};
	}

	public Control getCheckBoxControl() {
		return createCheckBoxControl(null);
	}

	private Control createCheckBoxControl(Composite parent) {
		if(checkBoxControl==null) {
			CheckBoxField checkBoxFild= new CheckBoxField(parent);
			checkBoxFild.addPropertyChangeListener(this);
			checkBoxControl = checkBoxFild.getCheckBox();
			if(getValue() instanceof Boolean) {
				checkBoxFild.getCheckBox().setSelection(((Boolean)getValue()).booleanValue());
			}
		} else if(parent!=null) {
			Assert.isTrue(checkBoxControl.getParent()==parent);
		}
		return checkBoxControl;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.internal.project.facet.IFieldEditor#isEditable()
	 */
	@Override
	public boolean isEditable() {
		return false;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.internal.project.facet.IFieldEditor#save(java.lang.Object)
	 */
	public void save(Object object) {
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.internal.project.facet.IFieldEditor#setEditable(boolean)
	 */
	@Override
	public void setEditable(boolean ediatble) {
	}

	@Override
	public void setValue(Object newValue) {
		((Button)checkBoxControl).setSelection(Boolean.parseBoolean(newValue.toString()));
		super.setValue(newValue);
	}

	@Override
	public void doFillIntoGrid(Object parent) {
	}

	public void propertyChange(PropertyChangeEvent evt) {
		setValue(evt.getNewValue());
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.BaseFieldEditor#getNumberOfControls()
	 */
	@Override
	public int getNumberOfControls() {
		return 1;
	}
}