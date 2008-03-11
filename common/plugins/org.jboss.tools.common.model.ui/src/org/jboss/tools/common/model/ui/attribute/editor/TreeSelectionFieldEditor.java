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
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class TreeSelectionFieldEditor extends ExtendedFieldEditor implements IFieldEditor, IPropertyFieldEditor, ISelectionChangedListener {

	protected TreeViewer treeViewer;
	protected int style = SWT.DEFAULT; //SWT.H_SCROLL | SWT.V_SCROLL | SWT.SINGLE | SWT.BORDER;

	protected IPropertyEditor propertyEditor;
	
	// ITreeEditor
	protected ILabelProvider labelProvider;
	protected ITreeContentProvider treeContentProvider;

	// ISelectionEditor
	protected ISelectionChangedListener selectionChangedListener;
	protected ISelectionProvider selectionProvider;

	public TreeSelectionFieldEditor() {}
	
	public TreeSelectionFieldEditor(IWidgetSettings settings) {
		super(settings);
	}

	///public TreeSelectionFieldEditor(String name, String labelText,	Composite parent, IWidgetSettings settings) {
	///	super(name, labelText, parent, settings);
	///}

	protected void adjustForNumColumns(int numColumns) {
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		getLabelComposite(parent);
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = numColumns;
	
		Control tree = createTreeControl(parent);
		gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = numColumns;
		tree.setLayoutData(gd);
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

	protected Control getTreeControl() {
		if (treeViewer!=null) return treeViewer.getControl();
		return null;
	}

	protected Control createTreeControl(Composite parent) {
		if (treeViewer == null) {
			treeViewer = new TreeViewer(parent, getStyle());
			treeViewer.setAutoExpandLevel(2);			
			// ITreeEditor
			treeViewer.setLabelProvider(labelProvider);
			treeViewer.setContentProvider(treeContentProvider);
			treeViewer.setInput(this);
			// ISelectionProvider
			if (selectionProvider!=null) {
				treeViewer.setSelection(selectionProvider.getSelection(), true);
				selectionProvider.addSelectionChangedListener(this);
			}
			if (selectionChangedListener!=null) {
				treeViewer.addSelectionChangedListener(selectionChangedListener);
			}
		}
		return treeViewer.getTree();
	}
	
	// getter and setter for style
	public int getStyle() {
		if (this.style!=SWT.DEFAULT) return style;
		return getSettings().getStyle("Table.Style");
	}
	public void setStyle(int i) {
		style = i;
	}

	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			labelProvider = (ILabelProvider)propertyEditor.getAdapter(ILabelProvider.class);
			treeContentProvider = (ITreeContentProvider)propertyEditor.getAdapter(ITreeContentProvider.class);
			selectionChangedListener = (ISelectionChangedListener)propertyEditor.getAdapter(ISelectionChangedListener.class);
			selectionProvider = (ISelectionProvider)propertyEditor.getAdapter(ISelectionProvider.class);
		}
	}

	// ISelectionChangedListener listen Selection Provider
	public void selectionChanged(SelectionChangedEvent event) {
		if (treeViewer != null) {
			treeViewer.setSelection(event.getSelection());
		}
	}

	// IFieldEditor
	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), createTreeControl(parent)};
	}

	public void setEnabled(boolean enabled){
		super.setEnabled(enabled);
		if (getTreeControl()!=null) getTreeControl().setEnabled(enabled);
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
