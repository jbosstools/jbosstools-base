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
import java.util.*;

import org.jboss.tools.common.model.ui.IListEditor;
import org.jboss.tools.common.model.ui.IStructuredChangeListener;
import org.jboss.tools.common.model.ui.IStructuredEditor;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.StructuredChange;
import org.jboss.tools.common.model.ui.StructuredChangedEvent;
import org.jboss.tools.common.model.ui.attribute.IListContentProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.StructuredListAdapter;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.util.Assert;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class ListStructuredFieldEditor extends ExtendedFieldEditor 
	implements IPropertyFieldEditor, IStructuredEditor, IListEditor, IFieldEditor, PropertyChangeListener 
{
	
	// IPropertyEditor
	protected IPropertyEditor propertyEditor;
	protected IValueProvider valueProvider;

	// IStructuredEditor
	protected IStructuredChangeListener structuredChangeListener;
//	protected IStructuredContentProvider structuredContentProvider;
	protected StructuredListAdapter.INewValueProvider newStructuredElementProvider;
	// IListEditor
	protected ILabelProvider labelProvider;
	protected IListContentProvider listContentProvider;

	// ListStructuredFieldEditor
	private List list;
	private Composite buttonBox;
	
	private static int ADD = 0, REMOVE = 1, UP = 2, DOWN = 3;
	private Button[] buttons = new Button[4];
	
	private SelectionListener selectionListener;
	private Composite mainPanel;
	private java.util.List<Object> elements;

	public ListStructuredFieldEditor() {}
		
	public ListStructuredFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	private void doAdd() {
		setPresentsDefaultValue(false);
		Object[] input = getNewInputObjects();
		for (int i = 0; i < input.length; i++) {
			String s = this.labelProvider.getText(input[i]);
			int index = list.getSelectionIndex();
			int target = -1;
			if (index >= 0) {
				elements.add(index + 1, input[i]);
				list.add(s, target = index + 1);
			} else {
				elements.add(i, input[i]);
				list.add(s, target = i);
			}
			selectionChanged(target);
		}
	}

	protected Object[] getNewInputObjects() {
		Object value = newStructuredElementProvider.getValue();
		Object[] values = (value == null) ? new Object[0] : (value instanceof Object[]) ? (Object[])value : new Object[]{value};  
		
		if (!allowDuplicates())	{
			Set<Object> set = new HashSet<Object>();
			ArrayList<Object> l = new ArrayList<Object>();
			set.addAll(elements);
			for (int i = 0; i < values.length; i++) {
				if(set.contains(values[i])) continue;
				l.add(values[i]);
				set.add(values[i]);
			}
			values = (Object[])l.toArray(new Object[0]);
		}
		
		return values;
	}

	protected void adjustForNumColumns(int numColumns) {
		Control control = getLabelComposite();
		((GridData)control.getLayoutData()).horizontalSpan = numColumns;
		((GridData)list.getLayoutData()).horizontalSpan = numColumns - 1;
	}

	private void createButtons(Composite parent) {
		buttons[ADD] = createButton(parent, "ListEditor.add");
		buttons[REMOVE] = createButton(parent, "ListEditor.remove");
		buttons[UP] = createButton(parent, "ListEditor.up");
		buttons[DOWN] = createButton(parent, "ListEditor.down");
	}

	private Button createButton(Composite parent, String key) {
		Button b = new Button(parent, SWT.PUSH);
		b.setText(JFaceResources.getString(key));
		b.setFont(parent.getFont());
		GridData d = new GridData(GridData.FILL_HORIZONTAL);
		d.heightHint = convertVerticalDLUsToPixels(b, 14/*IDialogConstants.BUTTON_HEIGHT*/);
		int widthHint = convertHorizontalDLUsToPixels(b, IDialogConstants.BUTTON_WIDTH);
		d.widthHint = Math.max(widthHint, b.computeSize(SWT.DEFAULT, SWT.DEFAULT, true).x);
		b.setLayoutData(d);
		b.addSelectionListener(getSelectionListener());
		return b;
	}

	public void createSelectionListener() {
		selectionListener = new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				if (event.widget == buttons[ADD]) {
					doAdd();
				} else if (event.widget == buttons[REMOVE]) {
					doRemove();
				} else if (event.widget == buttons[UP]) {
					doUp();
				} else if (event.widget == buttons[DOWN]) {
					doDown();
				} else if (event.widget == list) {
					selectionChanged(-1);
				}
			}
		};
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		Control control = getLabelComposite(parent);
		assignLabelLayoutData(control, numColumns);
		list = createListControl(parent);
		assignListLayoutData(list, numColumns);
		buttonBox = getButtonBoxControl(parent);
		assignButtonLayoutData(buttonBox);
	}
	private void assignLabelLayoutData(Control control, int numColumns) {
		GridData gd = new GridData();
		gd.horizontalSpan = numColumns;
		control.setLayoutData(gd);
	}
	private void assignListLayoutData(Control control, int numColumns) {
		GridData gd = new GridData(GridData.FILL_BOTH);
		//gd.verticalAlignment = GridData.FILL;
		gd.horizontalSpan = numColumns - 1;
		gd.grabExcessHorizontalSpace = true;
		control.setLayoutData(gd);
	}
	private void assignButtonLayoutData(Control control) {
		GridData gd = new GridData();
		gd.verticalAlignment = GridData.BEGINNING;
		control.setLayoutData(gd);
	}

	protected void doLoad() {
	}

	protected void doLoadDefault() {
	}

	protected void doStore() {
	}

	private void doDown() {
		swap(false);
	}

	protected Composite getButtonBoxControl(Composite parent) {
		if (buttonBox == null) {
			buttonBox = new Composite(parent, SWT.NULL);
			GridLayout layout = new GridLayout();
			layout.marginWidth = 0;
			buttonBox.setLayout(layout);
			createButtons(buttonBox);
			buttonBox.addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent event) {
					for (int i = 0; i < buttons.length; i++) buttons[i] = null;
					buttonBox = null;
				}
			});
	
		} else {
			checkParent(buttonBox, parent);
		}
	
		selectionChanged(0);
		return buttonBox;
	}

	protected Composite getMainPanel(Composite parent) {
		if (mainPanel == null) {
			mainPanel = new Composite(parent, SWT.NONE);
			GridLayout layout = new GridLayout(2, false);
			layout.marginWidth = 0;
			mainPanel.setLayout(layout);
			
			list = createListControl(mainPanel);
			GridData gd = new GridData(GridData.FILL_BOTH);
			//gd.verticalAlignment = GridData.FILL;
//			gd.horizontalSpan = numColumns - 1;
			gd.grabExcessHorizontalSpace = true;
			list.setLayoutData(gd);
		
			buttonBox = getButtonBoxControl(mainPanel);
			gd = new GridData();
			gd.verticalAlignment = GridData.BEGINNING;
			buttonBox.setLayoutData(gd);			
			updateSelectionDependentActions();
		} else
			checkParent(mainPanel, parent);			
		return mainPanel;
	}

	protected List getListControl() {
		return list;
	}

	protected List createListControl(Composite parent) {
		if (list == null) {
			list = new List(parent, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL);
			list.setFont(parent.getFont());
			list.addSelectionListener(getSelectionListener());
			list.addDisposeListener(new DisposeListener() {
				public void widgetDisposed(DisposeEvent event) {
					list = null;
				}
			});
			list.setItems(getItems());
		} else {
			if(mainPanel != null) {
				checkParent(mainPanel, parent);
			} else {
				checkParent(list, parent);
			}
		}
		return list;
	}

	private java.util.List<Object> createElements() {
		java.util.List<Object> elements = new ArrayList<Object>();
		if (this.listContentProvider!=null) 
			elements.addAll(Arrays.asList(listContentProvider.getElements(this)));
		return elements;
	}

	private String[] getItems() {
		if (this.elements==null) {
			elements = createElements();
		}
		Iterator i = elements.iterator();
		ArrayList<String> list = new ArrayList<String>();
		while (i.hasNext()) {
			Object o = i.next();
			String s = this.labelProvider.getText(o);
			list.add(s);
		}
		return list.toArray(new String[list.size()]);
	}

	public int getNumberOfControls() {
		return 2;
	}

	private SelectionListener getSelectionListener() {
		if (selectionListener == null)
			createSelectionListener();
		return selectionListener;
	}

	protected Shell getShell() {
		if (buttons[ADD] == null)
			return null;
		return buttons[ADD].getShell();
	}

	private void doRemove() {
		setPresentsDefaultValue(false);
		int index = list.getSelectionIndex();
		if (index >= 0) {
			list.remove(index);
			elements.remove(index);
			index = list.getItemCount() <= index ? list.getItemCount() - 1 : index;
			selectionChanged(index);
		}
	}

	private void selectionChanged(int newSelection) {
		// make StructuredChangedEvent and notify StructuredChangeListener
		StructuredChange change = new StructuredChange(elements);
		StructuredChangedEvent event = new StructuredChangedEvent(listContentProvider, change);
		structuredChangeListener.structureChanged(event);
		if(newSelection >= 0 && newSelection < list.getItemCount()) {
			list.select(newSelection);
		}
		updateSelectionDependentActions();
	}
	
	private void updateSelectionDependentActions() {
		int index = list.getSelectionIndex();
		int size = list.getItemCount();
		buttons[REMOVE].setEnabled(index >= 0);
		buttons[UP].setEnabled(size > 1 && index > 0);
		buttons[DOWN].setEnabled(size > 1 && index >= 0 && index < size - 1);
	}

	public void setFocus() {
		if (list != null) list.setFocus();
	}

	private void swap( boolean up) {
		setPresentsDefaultValue(false);
		int index = list.getSelectionIndex();
		int target = up ? index - 1 : index + 1;
	
		if (index >= 0) {
			String[] selection = list.getSelection();
			Assert.isTrue(selection.length == 1);
			list.remove(index);
			list.add(selection[0], target);
			list.setSelection(target);
			
			// elements
			Object o = elements.get(index); 
			elements.remove(index);
			elements.add(target, o);
		}
		selectionChanged(target);
	}

	private void doUp() {
		swap(true);
	}
	
	public void setEnabled(boolean enabled, Composite parent){
		super.setEnabled(enabled,parent);
		createListControl(parent).setEnabled(enabled);
		buttons[ADD].setEnabled(enabled);
		if(!enabled) {
			buttons[REMOVE].setEnabled(enabled);
			buttons[UP].setEnabled(enabled);
			buttons[DOWN].setEnabled(enabled);
		} else {
			updateSelectionDependentActions();
		}
	}
	
	// IStructuredEditor
	public void setStructuredChangeListener(IStructuredChangeListener structuredChangeListener) {
		this.structuredChangeListener = structuredChangeListener;
	}
	public void setStructuredContentProvider(IStructuredContentProvider structuredContentProvider) {
		//this.structuredContentProvider = structuredContentProvider;
	}
	public void setNewStructuredElementProvider(StructuredListAdapter.INewValueProvider valueProvider) {
		newStructuredElementProvider = valueProvider;
	}
	
	// IListEditor
	public void setLabelProvider(ILabelProvider labelProvider) {
		this.labelProvider = labelProvider;
	}
	public void setListContentProvider(IListContentProvider listContentProvider) {
		this.listContentProvider = listContentProvider;
//		this.structuredContentProvider = listContentProvider;
	}

	// IFieldEditor
	public Control[] getControls(Composite parent) {
//		return new Control[] {getLabelComposite(parent), getListControl(parent), getButtonBoxControl(parent)};
		return new Control[] {getLabelComposite(parent), getMainPanel(parent)};
	}
	
	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			// IStructuredEditor
			structuredChangeListener = (IStructuredChangeListener)propertyEditor.getAdapter(IStructuredChangeListener.class);
			listContentProvider = (IListContentProvider)propertyEditor.getAdapter(IListContentProvider.class);
			newStructuredElementProvider = (StructuredListAdapter.INewValueProvider)propertyEditor.getAdapter(StructuredListAdapter.INewValueProvider.class);
			// IListEditor
			labelProvider = (ILabelProvider)propertyEditor.getAdapter(ILabelProvider.class);
		}
		valueProvider.addValueChangeListener(this);
	}

	protected boolean allowDuplicates()
	{
		return false;
	}

	public void propertyChange(PropertyChangeEvent evt) {
		valueProvider.removeValueChangeListener(this);
		if (IPropertyEditor.VALUE.equals(evt.getPropertyName())) {
			elements = null;
			if(list!=null) {
				list.setItems(getItems());
				list.redraw();
			}
		}
		valueProvider.addValueChangeListener(this);
	}

	public void setEnabled(boolean enabled){
		super.setEnabled(enabled);
		if (getListControl()!=null) {
			getListControl().setEnabled(enabled);
		}
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
