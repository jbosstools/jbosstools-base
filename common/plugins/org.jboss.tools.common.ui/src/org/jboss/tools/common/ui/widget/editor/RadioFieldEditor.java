/*******************************************************************************
  * Copyright (c) 2007 - 2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.ui.widget.editor;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.ui.widget.field.RadioField;

/**
 * 
 * @author Viacheslav Kabanovich
 */
public class RadioFieldEditor extends BaseFieldEditor implements ITaggedFieldEditor,PropertyChangeListener{
	List values = null;
	List<String> labels;
	List<String> valueDescriptions;
	
	RadioField radioField;
	
	public RadioFieldEditor(String name, String label, List<String> labels, List values,Object defaultValue) {
		super(name, label, defaultValue);
		this.labels = labels;
		this.values = Collections.unmodifiableList(values);
	}

	public RadioFieldEditor(String name, String label, List<String> labels, List values,Object defaultValue, List<String> valueDescriptions) {
		this(name, label, labels, values, defaultValue);
		this.valueDescriptions = valueDescriptions;
	}

	@Override
	public Object[] getEditorControls(Object composite) {
		return new Control[] {getComboControl((Composite)composite)};
	}

	@Override
	public void doFillIntoGrid(Object parent) {
	}

	public Control getComboControl(Composite composite) {
		if(radioField == null) {
			radioField = new RadioField(composite,labels, values,getValue(), false, valueDescriptions);
			radioField.addPropertyChangeListener(this);
		} else if(composite!=null) {
			Assert.isTrue(radioField.getControl().getParent()==composite);
		}
		return radioField.getControl();
	}

	@Override
	public Object[] getEditorControls() {
		return new Control[]{radioField.getControl()};
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
		String[] result = new String[values.size()];
		for (int i = 0; i < result.length; i++) result[i] = values.get(0).toString();
		return result;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.ITaggedFieldEditor#setTags(java.lang.String[])
	 */
	public void setTags(String[] tags) {
		values.clear();
		for (int i = 0; i < tags.length; i++) values.add(tags[i]);
		//TODO
//		radioField.setTags(tags,getValueAsString());	
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.ui.widget.editor.BaseFieldEditor#getNumberOfControls()
	 */
	@Override
	public int getNumberOfControls() {
		return 1;
	}

	public void setValue(Object newValue) {
		if(newValue==null) {
			return;
		}
		super.setValue(newValue);
		if(radioField!=null) {
			radioField.removePropertyChangeListener(this);
			radioField.setValue(newValue.toString());
			radioField.addPropertyChangeListener(this);
		}
	}
}
