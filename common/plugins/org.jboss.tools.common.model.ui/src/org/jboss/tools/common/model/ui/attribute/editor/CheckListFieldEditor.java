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
import java.util.HashSet;
import java.util.Set;
import java.util.StringTokenizer;

import org.jboss.tools.common.model.ui.IAttributeErrorProvider;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.CheckListAdapter;
import org.jboss.tools.common.model.ui.wizards.query.list.TreeItemSelectionManager;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class CheckListFieldEditor extends ExtendedFieldEditor implements IFieldEditor, 
	IPropertyFieldEditor, IPropertyChangeListener, PropertyChangeListener {

	protected IPropertyEditor propertyEditor;
	protected IValueChangeListener valueChangeListener;
	protected IValueProvider valueProvider;
	protected IContentProvider contentProvider;
	protected ILabelProvider labelProvider;	
	
	private String stringValue;
	private TreeViewer viewer;

	String separator = ";";
	
	public CheckListFieldEditor() {}
	
	public CheckListFieldEditor(IWidgetSettings settings) {
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
		viewer = new TreeViewer(parent, SWT.CHECK);
		viewer.setContentProvider(contentProvider);
		viewer.setLabelProvider(labelProvider);
		viewer.setInput(contentProvider);
		
		new TreeItemSelectionManager(viewer, new Flipper());
		return viewer.getControl();
	}

	protected void init() {
		this.stringValue = valueProvider.getStringValue(true);
		setPropertyChangeListener(this);
		valueProvider.addValueChangeListener(this);
	}
	
	public void dispose() {
		super.dispose();
///		if (viewer!=null && viewer.getTree()!=null) viewer.getTree().removeMouseListener(mouseAdapter);
///		mouseAdapter = null;
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
		updateErrorState();
	}
	
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		this.propertyEditor = propertyEditor;
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
			contentProvider = (IContentProvider)propertyEditor.getAdapter(ITreeContentProvider.class);
			labelProvider = (ILabelProvider)propertyEditor.getAdapter(ILabelProvider.class);
//			Object input = propertyEditor.getInput();
			setErrorProvider((IAttributeErrorProvider)propertyEditor.getAdapter(IAttributeErrorProvider.class));
			if(propertyEditor.getInput() instanceof CheckListAdapter) {
				separator = "" + ((CheckListAdapter)propertyEditor.getInput()).getSeparator();
			}
		}
		init();
	}

	int lock = 0;
	public void propertyChange(PropertyChangeEvent event) {
		super.propertyChange(event);
		valueProvider.removeValueChangeListener(this);
		if (IPropertyEditor.VALUE.equals(event.getPropertyName())) {
			Object v = event.getNewValue();
			String s = (v == null) ? "" : v.toString(); //$NON-NLS-1$
			if(!s.equals(stringValue)) {
				stringValue = s;
				
				if(viewer == null || viewer.getTree() == null || viewer.getTree().isDisposed()) {
					//do nothing
				} else {
					if(lock == 0) {
						lock++;
						Tree tree = viewer.getTree();
						TreeItem[] is = tree.getItems();
						Set<String> vs = new HashSet<String>();
						StringTokenizer values = new StringTokenizer(stringValue, ";,"); //$NON-NLS-1$
						while(values.hasMoreTokens()) {
							String n = values.nextToken();
							vs.add(n);
						}
						for (int i = 0; i < is.length; i++) {
							Object d = is[i].getData();
							is[i].setChecked(vs.contains(d));
						}
						lock--;
					}
					viewer.refresh();
				}
			}
		}
		valueProvider.addValueChangeListener(this);
	}

	public void propertyChange(org.eclipse.jface.util.PropertyChangeEvent event) {
	}
	
	void flip0(TreeItem item) {
		if(item == null) return;

		String currentItem = item.getData().toString();
		StringTokenizer values = new StringTokenizer(valueProvider.getStringValue(true), ";,"); //$NON-NLS-1$
		String newValue = ""; //$NON-NLS-1$
		boolean currentItemExists = false;
		while (values.hasMoreTokens()) {
			String value = values.nextToken();
			if (value.equals(currentItem))
				currentItemExists = true;
			else {
				if(newValue.length() > 0 && !newValue.endsWith(";") && !newValue.endsWith(",")) newValue += separator; //$NON-NLS-1$ //$NON-NLS-2$
				newValue += value;
			}
		}
		if (!currentItemExists) {
			if(newValue.length() > 0 && !newValue.endsWith(";") && !newValue.endsWith(",")) newValue += separator; //$NON-NLS-1$ //$NON-NLS-2$
			newValue += currentItem;
		}
		valueChanged(newValue);
		viewer.refresh(currentItem);
	}

	class Flipper implements TreeItemSelectionManager.Listener {
		public void flip(TreeItem item) {
			if(lock > 0) return;
			lock++;
			flip0(item);
			lock--;
		}

		public boolean isSelected(Object data) {
			StringTokenizer values = new StringTokenizer(valueProvider.getStringValue(true), ";,"); //$NON-NLS-1$
			while (values.hasMoreTokens()) {
				String n = values.nextToken();
				if(data != null && data.equals(n)) return true;				
			}
			return false;
		}		
	}
	
	protected void valueChanged(String newValue) {
		String oldValue = stringValue;
		this.stringValue = newValue;
		PropertyChangeEvent event = new PropertyChangeEvent(this, IPropertyEditor.VALUE, oldValue, newValue);
		valueChangeListener.valueChange(event);
	}

	public void setFocus() {
		if(viewer != null && viewer.getTree() != null && !viewer.getTree().isDisposed()) {
			viewer.getTree().setFocus();
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
