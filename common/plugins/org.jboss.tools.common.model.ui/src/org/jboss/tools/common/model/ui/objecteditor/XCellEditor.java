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
package org.jboss.tools.common.model.ui.objecteditor;

import org.eclipse.core.runtime.ListenerList;
import org.jboss.tools.common.model.ui.attribute.adapter.AdapterFactory;
import org.jboss.tools.common.model.ui.attribute.editor.PropertyEditor;
import org.jboss.tools.common.model.ui.attribute.editor.PropertyEditorFactory;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellEditorListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;

public class XCellEditor extends CellEditor implements ICellEditorListener {
	XAttributeInfo info;
	CellEditor childEditor;
	PropertyEditor propertyEditor;
	Composite parent;
	ListenerList listeners = new ListenerList();

	public XCellEditor(Composite parent) {
		this.parent = parent;
	}

	protected Control createControl(Composite parent) {
		//ModelUIPlugin.log("XCellEditor.createControl()", new Exception());
		this.parent = parent;
		return null;
	}

	protected Object doGetValue() {
		//ModelUIPlugin.log("XCellEditor.doGetValue()", new Exception());
		if(childEditor == null) return info;
		info.setValue((String)childEditor.getValue());
		info.propertyEditor = propertyEditor;
		return info;
	}

	protected void doSetFocus() {
		//ModelUIPlugin.log("XCellEditor.doSetFocus()", new Exception());
		if(childEditor == null) return;
		childEditor.setFocus();
	}

	protected void doSetValue(Object value) {
		//ModelUIPlugin.log("XCellEditor.doSetValue()", new Exception());
		if(childEditor != null && childEditor.getControl()!=null && !childEditor.getControl().isDisposed()) {
			childEditor.removeListener(this);
			childEditor.dispose();
		}
		// model business
		info = (XAttributeInfo)value;
		XAttribute attribute = info.getObject().getModelEntity().getAttribute(info.getName());
		XModelObject modelObject = info.getObject();
		XModel model = info.getObject().getModel();  
		Object adapter = AdapterFactory.getAdapter(attribute, modelObject, model);
		propertyEditor = PropertyEditorFactory.createPropertyEditor(adapter, attribute, modelObject);
		childEditor = propertyEditor.getCellEditor(parent);
		childEditor.addListener(this);

		setValueValid(true);
	}
	
	public Control getControl() {
		//ModelUIPlugin.log("XCellEditor.create()", new Exception());
		if(childEditor == null) return null;		
		return childEditor.getControl();
	}
	
	public LayoutData getLayoutData() {
		//ModelUIPlugin.log("XCellEditor.getLayoutData()", new Exception());
		if(childEditor == null) return null;		
		return childEditor.getLayoutData();
	}
	
	public void create(Composite parent) {
		//ModelUIPlugin.log("XCellEditor.create()", new Exception());
		this.parent = parent;
		if(childEditor == null) return;		
		childEditor.create(parent);
	}
	
	public void activate() {
		//ModelUIPlugin.log("XCellEditor.activate()", new Exception());
		if(childEditor == null) return;		
		childEditor.activate();
	}

	public void dispose() {
		//ModelUIPlugin.log("XCellEditor.dispose()", new Exception());
		if(childEditor == null) return;		
		childEditor.dispose();
	}

	public void deactivate() {
		//ModelUIPlugin.log("XCellEditor.deactivate()", new Exception());
		if(childEditor == null) return;
		this.propertyEditor.fireValueChanged();
		childEditor.deactivate();
	}

	public void addListener(ICellEditorListener listener) {
		//ModelUIPlugin.log("XCellEditor.addListener()", new Exception());
		listeners.add(listener);
	}
	
	public void removeListener(ICellEditorListener listener) {
		//ModelUIPlugin.log("XCellEditor.removeListener()", new Exception());
		listeners.remove(listener);
	}

	// ICellEditorListener
	public void applyEditorValue() {
		//ModelUIPlugin.log("XCellEditor.applyEditorValue()", new Exception());
		Object[] list = listeners.getListeners();
		if ((list!=null) && (list.length>0)) {
			for (int i=0;i<list.length;++i) ((ICellEditorListener)list[i]).applyEditorValue();
		}
	}

	public void cancelEditor() {
		//ModelUIPlugin.log("XCellEditor.cancelEditor()", new Exception());
		Object[] list = listeners.getListeners();
		if ((list!=null) && (list.length>0)) {
			for (int i=0;i<list.length;++i) ((ICellEditorListener)list[i]).cancelEditor();
		}
	}

	public void editorValueChanged(boolean oldValidState, boolean newValidState) {
		//ModelUIPlugin.log("XCellEditor.editorValueChanged()", new Exception());
		Object[] list = listeners.getListeners();
		if ((list!=null) && (list.length>0)) {
			for (int i=0;i<list.length;++i) ((ICellEditorListener)list[i]).editorValueChanged(oldValidState, newValidState);
		}
	}

}
