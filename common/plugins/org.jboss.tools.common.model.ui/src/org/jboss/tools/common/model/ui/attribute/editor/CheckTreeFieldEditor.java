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

import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.wizards.query.list.TreeItemSelectionManager;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TreeItem;

import org.jboss.tools.common.model.ui.viewers.xpl.ICheckable;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

/**
 * @author au
 */

public class CheckTreeFieldEditor extends ExtendedFieldEditor implements IFieldEditor, 
	IPropertyFieldEditor, IPropertyChangeListener, PropertyChangeListener {

	protected IPropertyEditor propertyEditor;
	protected IValueChangeListener valueChangeListener;
	protected IValueProvider valueProvider;
	protected IContentProvider contentProvider;
	protected ILabelProvider labelProvider;
	// ISelectionEditor
	protected ISelectionChangedListener selectionChangedListener;
	protected ISelectionProvider selectionProvider;
	
	private String stringValue;
	private TreeViewer viewer;
	
	public CheckTreeFieldEditor() {}
	
	public CheckTreeFieldEditor(IWidgetSettings settings) {
		super(settings);	
	}

	protected void adjustForNumColumns(int numColumns) {
		Control control = getLabelComposite();
		((GridData)control.getLayoutData()).horizontalSpan = numColumns;
		((GridData)viewer.getControl().getLayoutData()).horizontalSpan = numColumns - 1;
	}

	protected void doFillIntoGrid(Composite parent, int numColumns) {
		Control control = getLabelComposite(parent);
		GridData gd = new GridData();
		gd.horizontalSpan = numColumns;
		gd.verticalAlignment = GridData.GRAB_VERTICAL;
		control.setLayoutData(gd);
		getListControl(parent);

		gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = numColumns;
		gd.grabExcessHorizontalSpace = true;
		viewer.getControl().setLayoutData(gd);
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

	public Control[] getControls(Composite parent) {
		return new Control[] {getLabelComposite(parent), getListControl(parent)};
	}
	
	private Control getListControl(Composite parent) {
		if(viewer != null && !viewer.getControl().isDisposed()) return viewer.getControl();
		viewer = new TreeViewer(parent);
		viewer.setAutoExpandLevel(2);
		viewer.setContentProvider(contentProvider);
		viewer.setLabelProvider(labelProvider);
		viewer.setInput(contentProvider);
		new TreeItemSelectionManager(viewer, new Flipper());
		// ISelectionProvider
		if (selectionProvider!=null) {
			viewer.setSelection(selectionProvider.getSelection(), true);
			//selectionProvider.addSelectionChangedListener(this);
		}
		if (selectionChangedListener!=null) {
			viewer.addSelectionChangedListener(selectionChangedListener);
		}
		return viewer.getControl();
	}

	protected void init() {
		this.stringValue = valueProvider.getStringValue(true);
	}

	public void dispose() {
		super.dispose();
//		if (viewer!=null && viewer.getTree()!=null) viewer.getTree().removeMouseListener(mouseAdapter);
//		mouseAdapter = null;
		if (viewer!=null && viewer.getTree()!=null && !viewer.getTree().isDisposed()) viewer.getTree().dispose();
		viewer = null;
		propertyEditor = null;
		valueChangeListener = null;
		valueProvider = null;
		contentProvider = null;
		labelProvider = null;
	}
	
	public void setEnabled(boolean enabled){
		super.setEnabled(enabled);
	}
	
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
			contentProvider = (IContentProvider)propertyEditor.getAdapter(ITreeContentProvider.class);
			labelProvider = (ILabelProvider)propertyEditor.getAdapter(ILabelProvider.class);
			selectionChangedListener = (ISelectionChangedListener)propertyEditor.getAdapter(ISelectionChangedListener.class);
			selectionProvider = (ISelectionProvider)propertyEditor.getAdapter(ISelectionProvider.class);
//			Object input = propertyEditor.getInput();
		}
		init();
	}

	public void propertyChange(PropertyChangeEvent event) {
	}

	public void propertyChange(org.eclipse.jface.util.PropertyChangeEvent event) {
	}

	class Flipper implements TreeItemSelectionManager.Listener {
		public void flip(TreeItem item) {
			Object checkItem = item.getData();
			if (checkItem instanceof ICheckable) {
				((ICheckable)checkItem).toggle(checkItem);
			}
			viewer.refresh();
		}		
	}
	
	protected void valueChanged(String newValue) {
		String oldValue = stringValue;
		this.stringValue = newValue;
		PropertyChangeEvent event = new PropertyChangeEvent(this, IPropertyEditor.VALUE, oldValue, newValue);
		valueChangeListener.valueChange(event);
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
