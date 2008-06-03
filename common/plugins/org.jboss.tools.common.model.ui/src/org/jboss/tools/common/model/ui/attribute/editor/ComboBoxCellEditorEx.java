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

import java.text.MessageFormat;

import org.eclipse.jface.util.Assert;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CCombo;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * @author aleksey
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class ComboBoxCellEditorEx extends CellEditor {
	
	private CCombo combo;
	private String[] items = new String[0];
	private Object value = null;
	private int selection;
	private boolean skipDeactivate = Boolean.FALSE.booleanValue();

	public ComboBoxCellEditorEx() {
		super();
	}

	public ComboBoxCellEditorEx(Composite parent) {
		create(parent);
	}

	public ComboBoxCellEditorEx(Composite parent, int style) {
		this.setStyle(style);
		create(parent);
	}

	public ComboBoxCellEditorEx(Composite parent, String[] items, int style) {
		if (items!=null) this.items = items;
		this.setStyle(style);
		create(parent);
	}

	public void activate() {
		super.activate();
	}

	public void create(Composite parent) {
		super.create(parent);
	}
	
	public void deactivate() {
		if (!this.skipDeactivate) super.deactivate();
	}

	protected Control createControl(Composite parent) {
		combo = new CCombo(parent, getStyle());
		combo.setItems(items);
		combo.setFont(parent.getFont());

		combo.addKeyListener(new KeyAdapter() {
			// hook key pressed - see PR 14201  
			public void keyPressed(KeyEvent e) {
				keyReleaseOccured(e);
			}
		});

		combo.addSelectionListener(new SelectionAdapter() {
			public void widgetDefaultSelected(SelectionEvent event) {
				widgetSelected(event);
			}
			public void widgetSelected(SelectionEvent event) {
				if(!isActivated()) return;
				int i = combo.getSelectionIndex();
				if(i >= 0) combo.setText(combo.getItem(i));
				applyEditorValueAndDeactivate();
			}
		});

		combo.addTraverseListener(new TraverseListener() {
			public void keyTraversed(TraverseEvent e) {
				if (e.detail == SWT.TRAVERSE_ESCAPE || e.detail == SWT.TRAVERSE_RETURN) {
					e.doit = false;
				}
			}
		});
		
		combo.addFocusListener(new FocusListener() {
			public void focusGained(FocusEvent e) {
			}
			public void focusLost(FocusEvent e) {
				ComboBoxCellEditorEx.this.focusLost();
			}
		});
		
		combo.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				valueChanged(true, true);
			}
		});

		setValueValid(true);

		return combo;
	}

	protected Object doGetValue() {
		return this.value;
	}

	protected void doSetFocus() {
		if (combo!=null && !combo.isDisposed())	combo.setFocus();
	}

	public void focusLost() {
		value = combo.getText();
		valueChanged(true, true);
		this.fireApplyEditorValue();
		super.focusLost();
	}

	public Control getControl() {
		return super.getControl();
	}

	///fix bug 8473
	///private boolean forceFocus = Boolean.FALSE.booleanValue();
	
	protected void fireApplyEditorValue() {
		super.fireApplyEditorValue();
		///if (forceFocus) return;
		///forceFocus = Boolean.TRUE.booleanValue();
		///if(combo != null && !combo.isDisposed()) combo.forceFocus();
		///forceFocus = Boolean.FALSE.booleanValue();
	}
	
	protected void fireCancelEditor() {
		skipDeactivate = Boolean.TRUE.booleanValue();
		super.fireCancelEditor();
		skipDeactivate = Boolean.FALSE.booleanValue();
		deactivate();
	}
	


	protected void doSetValue(Object value) {
		this.value = value;
		int selection;
		if (value instanceof Integer) {
			selection = ((Integer) value).intValue();
			combo.select(selection);
		}
		if (value instanceof String) {
			selection = findIndex(value);
			if(selection < 0) combo.setText(value.toString());
			else combo.select(selection);
		}
		doSetFocus();
	}
	
	private int findIndex(Object value) {
		String[] items = getItems();
		if(items == null) return 0;
		for (int i = 0;i < items.length; i++) {
			if(items[i].equals(value)) return i;
		}
		return -1;
	}

	public String[] getItems() {
		return this.items;
	}

	public void setItems(String[] items) {
		Assert.isNotNull(items);
		this.items = items;
		populateComboBoxItems();
	}

	private void populateComboBoxItems() {
		if (combo != null && items != null) {
			combo.removeAll();
			for (int i = 0; i < items.length; i++) combo.add(items[i], i);
			setValueValid(true);
			selection = 0;
		}
	}

	private void applyEditorValueAndDeactivate() {
		Object newValue = combo.getText();
		markDirty();
		boolean isValid = isCorrect(newValue);
		setValueValid(isValid);
		if (!isValid) {
			// try to insert the current value into the error message.
			setErrorMessage(
				MessageFormat.format(getErrorMessage(), new Object[] {items[selection]})); 
		} else {
			this.value = newValue;
		}
		fireApplyEditorValue();
		deactivate();
	}
}
