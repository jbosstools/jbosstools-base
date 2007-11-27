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

import java.beans.PropertyChangeListener;
import java.util.Arrays;

import org.jboss.tools.common.model.ui.IAttributeErrorProvider;
import org.jboss.tools.common.model.ui.IListEditor;
import org.jboss.tools.common.model.ui.IListValueAdapter;
import org.jboss.tools.common.model.ui.ISelectionEditor;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueEditor;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.attribute.IListContentProvider;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class ListSelectionFieldEditor extends ExtendedFieldEditor implements IFieldEditor, IValueEditor, IListEditor, ISelectionEditor, IPropertyFieldEditor, IPropertyChangeListener, PropertyChangeListener {
	protected ListViewer listViewer;
	protected int style = SWT.H_SCROLL | SWT.V_SCROLL | SWT.SINGLE | SWT.BORDER;

	// IValueEditor
	protected IValueChangeListener valueChangeListener;
	protected IValueProvider valueProvider;
	
	// IListEditor
	protected ILabelProvider labelProvider;
	protected IListContentProvider listContentProvider;
	protected IListValueAdapter listValueAdapter;
	// ISelectionEditor
	protected ISelectionProvider selectionProvider;
	protected ISelectionChangedListener selectionChangedListener;

	public ListSelectionFieldEditor() {}
	
	public ListSelectionFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	//public ListSelectionFieldEditor(String name, String labelText,	Composite parent, IWidgetSettings settings) {
	//	super(name, labelText, parent, settings);
	//}

	protected void adjustForNumColumns(int numColumns) {

	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		getLabelComposite(parent);
		numColumns--;
		
		Control list = createListControl(parent);
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = numColumns;
		list.setLayoutData(gd);
	}

	protected void doLoad() {
	}

	protected void doLoadDefault() {
	}

	protected void doStore() {
	}

	public int getNumberOfControls() {
		return 2;
	}

	protected Control getListControl() {
		if (listViewer!=null) return listViewer.getList();
		return null;
	}

	protected Control createListControl(Composite parent) {
		listViewer = new ListViewer(parent, getStyle());
		// init model
		listViewer.setLabelProvider(labelProvider);
		listViewer.setContentProvider(listContentProvider);
		if (selectionProvider!=null) {
			listViewer.setSelection(selectionProvider.getSelection()); // its set value
		}
		if (selectionChangedListener!=null) {
			listViewer.addPostSelectionChangedListener(selectionChangedListener);
		}
		listViewer.setInput(this);
		if(valueProvider != null) {
			String v = ("" + valueProvider.getValue()).toString();
			listViewer.setSelection(new StructuredSelection(v));
		}
		listViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				valueChanged();
			}			
		});
		return listViewer.getList();
	}

	public int getStyle() {
		return style;
	}

	public void setStyle(int i) {
		style = i;
	}
	
	// ISelectionEditor
	public void setSelectionChangedListener(ISelectionChangedListener selectionChangedListener) {
		this.selectionChangedListener = selectionChangedListener;
	}
	public void setSelectionProvider(ISelectionProvider selectionProvider) {
		this.selectionProvider = selectionProvider;
	}

	// IListEditor
	public void setLabelProvider(ILabelProvider labelProvider) {
		this.labelProvider = labelProvider;
	}
	public void setListContentProvider(IListContentProvider listContentProvider) {
		this.listContentProvider = listContentProvider;
	}
	public void setListValueAdapter(IListValueAdapter listValueAdapter) {
		this.listValueAdapter = listValueAdapter;
	}
	
	// IValueEditor
	public void setValueChangeListener(IValueChangeListener valueChangeListener) {
		this.valueChangeListener = valueChangeListener;
	}
	public void setValueProvider(IValueProvider valueProvider) {
		this.valueProvider = valueProvider;
	}

	// IFieldEditor
	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), createListControl(parent)};
	}
	
	public void setEnabled(boolean enabled){

		super.setEnabled(enabled);
		if (getListControl()!=null) {
			getListControl().setEnabled(enabled);
		}
	}

	public void cut() {}

	public void copy() {}

	public void paste() {}

	public void delete() {
	}

	IPropertyEditor propertyEditor;

	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
			labelProvider = (ILabelProvider)propertyEditor.getAdapter(ILabelProvider.class);
			listContentProvider = (IListContentProvider)propertyEditor.getAdapter(IListContentProvider.class);
			setErrorProvider((IAttributeErrorProvider)propertyEditor.getAdapter(IAttributeErrorProvider.class));
		}
		setPropertyChangeListener(this);
		valueProvider.addValueChangeListener(this);
	}

	public void propertyChange(PropertyChangeEvent event) {
		if(ExtendedFieldEditor.VALUE.equals(event.getProperty())) {
			setPropertyChangeListener(null);
			java.beans.PropertyChangeEvent e = new java.beans.PropertyChangeEvent(this, IPropertyEditor.VALUE, mapFromTo(tags,elements,event.getOldValue()), mapFromTo(tags,elements,event.getNewValue()));
			valueChangeListener.valueChange(e);
			setPropertyChangeListener(this);
		}
	}

	public void propertyChange(java.beans.PropertyChangeEvent evt) {
		if (IPropertyEditor.VALUE.equals(evt.getPropertyName())) {
			Object v = evt.getNewValue();
			valueProvider.removeValueChangeListener(this);
			if(v != null) {
				listViewer.setSelection(new StructuredSelection(v));
			} else {
				listViewer.setSelection(new StructuredSelection());
			}
			valueProvider.addValueChangeListener(this);
		}
	}
	
	Object[] elements;
	String[] tags = new String[0];
	
	protected String[] getTags() {
		elements = listContentProvider.getElements(this);
		tags = new String[elements.length];
		for(int i=0;i<elements.length;++i){ 
			tags[i] = labelProvider.getText(elements[i]);
		}
		return tags;
	}

	static private Object mapFromTo(Object[] from, Object[] to,Object value) {
		if(from==null || from.length==0 || to==null || to.length==0) return value;
		int index = Arrays.asList(from).indexOf(value);
		return index==-1?value:to[index];
	}
	
	protected void valueChanged() {
		setPresentsDefaultValue(false);
		String oldValue = "";
		String newValue = getSelection();
		this.valueProvider.removeValueChangeListener(this);
		java.beans.PropertyChangeEvent event = new java.beans.PropertyChangeEvent(this, IPropertyEditor.VALUE, oldValue, newValue);
		valueChangeListener.valueChange(event);
		this.valueProvider.addValueChangeListener(this);
	}
	
	String getSelection() {
		ISelection s = listViewer.getSelection();
		if(s == null || s.isEmpty()) return "";
		if(!(s instanceof StructuredSelection)) return "";
		return ((StructuredSelection)s).getFirstElement().toString();
	}

}
