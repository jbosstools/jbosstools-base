/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.ui.widget.editor;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.ui.CommonUIMessages;

/**
 * @author eskimo
 *
 */
public class CompositeEditor extends BaseFieldEditor implements PropertyChangeListener {

	public CompositeEditor(String name, String label, Object defaultValue) {
		super(name, label, defaultValue);
	}

	@Override
	public void doFillIntoGrid(Object parent) {
		Assert.isTrue(parent instanceof Composite,
				CommonUIMessages.COMPOSITE_EDITOR_PARENT_CONTROL_SHOULD_BE_COMPOSITE);
		Assert.isTrue(((Composite) parent).getLayout() instanceof GridLayout,
				CommonUIMessages.COMPOSITE_EDITOR_EDITOR_SUPPORTS_ONLY_GRID_LAYOUT);

		Composite aComposite = (Composite) parent;
		final Control[] controls = (Control[]) getEditorControls(aComposite);
		GridLayout gl = (GridLayout) ((Composite) parent).getLayout();

        for (int i = 0; i < controls.length; i++) {
			GridData gd = new GridData();
			gd.horizontalSpan = i == 1 ? gl.numColumns - controls.length + 1 : 1;
			if (controls[i] instanceof Combo && i == (controls.length - 1)) {
				gd.horizontalAlignment = SWT.BEGINNING;
			} else {
				gd.horizontalAlignment = GridData.FILL;
				gd.grabExcessHorizontalSpace = (i == 1);				
			}

			controls[i].setLayoutData(gd);
			controls[i].setEnabled(isEnabled());

			if(i==0) {
				controls[i].addDisposeListener(new DisposeListener(){
					public void widgetDisposed(DisposeEvent e) {
						dispose();
						controls[0].removeDisposeListener(this);
					}
				});
			}
        }
	}

	List<Control> controls = new ArrayList<Control>();

	@Override
	public Object[] getEditorControls() {
			if(!controls.isEmpty()) return controls.toArray();
			else throw new IllegalStateException(CommonUIMessages.COMPOSITE_EDITOR_THIS_METOD_CAN_BE_INVOKED);
	}

	@Override
	public Object[] getEditorControls(Object parent) {
		for (IFieldEditor editor : editors) {
			controls.addAll(Arrays.asList((Control[])editor.getEditorControls(parent)));
		}
		return controls.toArray(new Control[]{});
	}

	@Override
	public int getNumberOfControls() {
		return editors.size();
	}

	@Override
	public boolean isEditable() {
		return true;
	}

	public void save(Object object) {
	}

	@Override
	public void setEditable(boolean ediatble) {
	}

	List<IFieldEditor> editors = new ArrayList<IFieldEditor>();

	public CompositeEditor addFieldEditors(IFieldEditor[] editors) {
		this.editors.addAll( Arrays.asList(editors));
		for (IFieldEditor editor : Arrays.asList(editors)) {
			editor.addPropertyChangeListener(this);
		}
		return this;
	}

	@Override
	public void setValue(Object newValue) {
		for (IFieldEditor editor : editors) {
			editor.removePropertyChangeListener(this);
			editor.setValue(newValue);
			editor.addPropertyChangeListener(this);
		}
		super.setValue(newValue);
	}

	public void propertyChange(PropertyChangeEvent event) {
		for (IFieldEditor editor : editors) {
			if(event.getSource()!=editor) {
				editor.removePropertyChangeListener(this);
				editor.setValue(event.getNewValue());
				editor.addPropertyChangeListener(this);				
			}
		}
		super.setValue(event.getNewValue());
	}

	@Override
	public void setEnabled(boolean set) {
		for (IFieldEditor editor : editors) {
			editor.setEnabled(set);
		}
	}

	public List<IFieldEditor> getEditors() {
		return Collections.unmodifiableList(editors);
	}

	public IFieldEditor getEditorByName(String name) {
		for (IFieldEditor editor : editors) {
			if(name.equals(editor.getName())) {
				return editor;
			}
		}
		return null;
	}

	public void setData(Object key, Object value) {
		super.setData(key, value);
		for (IFieldEditor editor : editors) {
			editor.setData(key, value);
		}
	}
}