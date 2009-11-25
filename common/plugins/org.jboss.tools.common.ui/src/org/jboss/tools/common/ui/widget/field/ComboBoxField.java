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
package org.jboss.tools.common.ui.widget.field;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;

/**
 * @author eskimo
 *
 */
public class ComboBoxField extends BaseField implements ISelectionChangedListener {

	ComboViewer comboControl = null;
	List values = new ArrayList();
	
	public ComboBoxField(Composite parent,List values, ILabelProvider labelProvider, 
			Object value, boolean flatStyle) { 
		this(parent, values, value, flatStyle);
		comboControl.setLabelProvider(labelProvider);
	}

	public ComboBoxField(Composite parent,List values, Object value, boolean editable) {
		this.values = values;
		/*
		 * Used combo box instead of custom combobox(CCombo), 
		 * CCombo looks ugly under MAC OS X
		 */
		Combo combo;
		if(editable==true) {
		
			combo = new Combo(parent, SWT.DROP_DOWN);
		} else {
			combo = new Combo(parent, SWT.READ_ONLY);
		}
		combo.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_WHITE));
		comboControl = new ComboViewer(combo);
		comboControl.setContentProvider(new IStructuredContentProvider() {

			public void dispose() {			
			}

			public void inputChanged(Viewer viewer, Object oldInput,
					Object newInput) {
			}

			public Object[] getElements(Object inputElement) {
				return ComboBoxField.this.values.toArray();
			}
		});

		comboControl.addSelectionChangedListener(this);
		comboControl.getCombo().addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				firePropertyChange(new Object(), comboControl.getCombo().getText());
			}});
		comboControl.setLabelProvider(new ILabelProvider() {
			public void addListener(ILabelProviderListener listener) {			
			}

			public void dispose() {		
			}

			public boolean isLabelProperty(Object element, String property) {
				return false;
			}

			public void removeListener(ILabelProviderListener listener) {
			}

			public Image getImage(Object element) {
				return null;
			}

			public String getText(Object element) {
				return element.toString();
			}
		});
		comboControl.setInput(values);
		comboControl.setSelection(new StructuredSelection(value), true);
	}

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public void selectionChanged(SelectionChangedEvent event) {
		firePropertyChange("", ((StructuredSelection)event.getSelection()).getFirstElement()); //$NON-NLS-1$
	}

	public Combo getComboControl() {
		return comboControl.getCombo();
	}

	@Override
	public Control getControl() {
		return getComboControl();
	}

	public void setValue(Object newValue) {
		comboControl.setSelection(new StructuredSelection(newValue));
		comboControl.getCombo().setText(newValue.toString());
	}

	public void setTags(String[] tags,String value) {
		values = Arrays.asList(tags);
		comboControl.removeSelectionChangedListener(this);
		comboControl.refresh(true);
		comboControl.addPostSelectionChangedListener(this);
		comboControl.setSelection(new StructuredSelection(value));
	}
	/*
	 * We can't modify combo, change style of this object, 
	 * if we such functionality please use CCombo
	 * 
	 */
	public void setEditable(
			boolean ediatble) {
		//comboControl.getCCombo().setEditable(false);
	}
}