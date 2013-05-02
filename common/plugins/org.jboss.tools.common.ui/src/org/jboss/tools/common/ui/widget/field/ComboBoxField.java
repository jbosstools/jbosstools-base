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
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
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
	boolean modifyLock = false;
	
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
			combo = new Combo(parent, SWT.NONE);
			new MouseHandler(combo);
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
				if(modifyLock) return;
				modifyLock = true;
				try {
					firePropertyChange(new Object(), comboControl.getCombo().getText());
				} finally {
					modifyLock = false;
				}
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
		if(modifyLock) return;
		comboControl.setSelection(new StructuredSelection(newValue));
		modifyLock = true;
		try {
			comboControl.getCombo().setText(newValue.toString());
		} finally {
			modifyLock = false;
		}
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

class MouseHandler extends MouseAdapter implements MouseMoveListener {
	Combo combo;
	boolean isSettingFocus = false;
	int basePosition = -1;

	public MouseHandler(Combo combo) {
		this.combo = combo;
		combo.addMouseListener(this);
		combo.addMouseMoveListener(this);
	}

	public void mouseDown(MouseEvent e) {
		if(isSettingFocus) return;
		isSettingFocus = true;
		try {
			if(!combo.isDisposed()) {
				combo.setFocus();
				int i = getPosition(e.x);
				combo.setSelection(new Point(i,i));
				basePosition = i;
			}
		} finally {
			isSettingFocus = false;
		}
	}
	public void mouseDoubleClick(MouseEvent e) {
		if(!combo.isDisposed()) {
			combo.setSelection(new Point(0, combo.getText().length()));
		}
	}
	public void mouseUp(MouseEvent e) {
		basePosition = -1;
	}

	public void mouseMove(MouseEvent e) {
		if(basePosition < 0 || combo.isDisposed()) return;
		int i = getPosition(e.x);
		combo.setSelection(new Point(basePosition,i));
	}

	private int getPosition(int x) {
		int result = 0;
		GC g = new GC(combo);
		String text = combo.getText();
		int cp = combo.getCaretPosition();
		if(cp >= 0) {
			x -= combo.getCaretLocation().x - g.stringExtent(text.substring(0, cp)).x;
		}
		int n = text.length();		
		int width = g.stringExtent(text.substring(0, n)).x;
		if(x < 3) {
			result = 0;
		} else if(width < x) {
			result = n;
		} else {
			int c1 = 0;
			int c2 = n;
			while(c2 > c1) {
				int c = (c2 + c1 + 1) / 2;
				int xi = g.stringExtent(text.substring(0, c)).x;
				int w = g.stringExtent(text.substring(c - 1, c)).x;
				if(xi - w < x + 3 && xi > x - 1) {
					if(xi - w / 2 > x + 3) result = c - 1; else result = c;
					break;
				}
				if(c == c2) {
					result = c;
					break;
				}
				if(x > xi) {
					c1 = c;
				} else {
					c2 = c;
				}
			}
		}
		g.dispose();
		return result;
	}

}
