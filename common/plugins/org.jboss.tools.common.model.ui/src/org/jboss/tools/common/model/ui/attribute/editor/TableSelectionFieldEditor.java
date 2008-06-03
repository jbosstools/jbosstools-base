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

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class TableSelectionFieldEditor extends ExtendedFieldEditor implements IPropertyFieldEditor, IFieldEditor {

	protected IPropertyEditor propertyEditor;
	protected TableViewer tableViewer;
	
	//protected Table table;
	
	protected IStructuredContentProvider structuredContentProvider;
	protected ILabelProvider labelProvider;
	protected ISelectionProvider selectionProvider;
	protected ISelectionChangedListener selectionChangedListener;
	
	public TableSelectionFieldEditor() {}
	
	public TableSelectionFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	// FieldEditor
	protected void adjustForNumColumns(int numColumns) {
	}
	protected void doFillIntoGrid(Composite parent, int numColumns) {
		Control control = this.getLabelComposite(parent);
		GridData gd = new GridData();
		gd.horizontalSpan = numColumns;
		control.setLayoutData(gd);

		control = this.createTableControl(parent);
		gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = numColumns;
		control.setLayoutData(gd);
	}
	protected void doLoad() {
	}
	protected void doLoadDefault() {
	}
	protected void doStore() {
	}
	public int getNumberOfControls() {
		return 1;
	}
	
	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			setLabelText(propertyEditor.getLabelText());
			structuredContentProvider = (IStructuredContentProvider)propertyEditor.getAdapter(IStructuredContentProvider.class);
			labelProvider = (ILabelProvider)propertyEditor.getAdapter(ILabelProvider.class);
			selectionProvider = (ISelectionProvider)propertyEditor.getAdapter(ISelectionProvider.class);
			selectionChangedListener = (ISelectionChangedListener)propertyEditor.getAdapter(ISelectionChangedListener.class);
		}
	}

	// IFieldEditor
	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), createTableControl(parent)};
	}

	protected Control getTableControl() {
		if (tableViewer!=null) {
			return tableViewer.getControl();
		}
		return null;
	}

	protected Control createTableControl(Composite parent) {
		if (tableViewer==null) {
			tableViewer = new TableViewer(parent, SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER );

			//tableViewer.setColumnProperties(new String[] {"Key1","Key2"});
			//tableViewer.setCellModifier(new CM());
			//tableViewer.setCellEditors(new CellEditor[]{null, new TextCellEditor(tableViewer.getTable(), SWT.NONE)});

			tableViewer.setLabelProvider(labelProvider);
			tableViewer.setContentProvider(structuredContentProvider);
			tableViewer.setInput(structuredContentProvider);
		
			tableViewer.addSelectionChangedListener(selectionChangedListener);
			tableViewer.setSelection(selectionProvider.getSelection());
		}
		return tableViewer.getControl();
	}

	public void setEnabled(boolean enabled){
		super.setEnabled(enabled);
		if (getTableControl()!=null) {
			getTableControl().setEnabled(enabled);
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
