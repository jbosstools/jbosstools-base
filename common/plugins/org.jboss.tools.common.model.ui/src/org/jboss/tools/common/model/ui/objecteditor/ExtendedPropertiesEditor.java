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

import java.util.Properties;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TableItem;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class ExtendedPropertiesEditor {
	static Color DEFAULT_COLOR = new Color(null, 0, 0, 0);
	static Color DISABLED_COLOR = new Color(null, 127, 127, 127);
	static Image EMPTY = EclipseResourceUtil.getImage("images/actions/empty.gif");
	protected XTable xtable = new XTable();
	protected ExtendedProperties attributes;
	protected ICellModifier modifier = new CellModifierImpl();
	protected boolean readOnly = false;
	ExtendedCellEditorProvider cellEditorProvider = null;
	Properties context = new Properties();
	
	public ExtendedPropertiesEditor() {
		xtable.setTableProvider(new XTableProviderImpl());
	}
	
	public void setExtendedProperties(ExtendedProperties attributes) {
		if(this.attributes == attributes) return;
		stopEditing();		
		this.attributes = attributes;
		if(attributes != null) {
			context.setProperty("nodeName", "" + attributes.getNodeName());
			cellEditorProvider = attributes.createCellEditorProvider();
		}
		if(xtable.getTable() == null || xtable.getTable().isDisposed()) return; 
		xtable.getTable().setSelection(-1);
		update();	
	}
	
	public void setReadOnly(boolean b) {
		readOnly = b;
	}

	public Control createControl(Composite parent) {
		return createControl(parent, xtable.style);
	}
	
	public Control createControl(Composite parent, int style) {
		xtable.createControl(parent, style);
		xtable.getViewer().setColumnProperties(new String[]{"name", "value"});
		xtable.getViewer().setCellModifier(modifier);
		if(!readOnly) {
			CellEditor cellEditor = (cellEditorProvider != null) 
				? cellEditorProvider.createCellEditor(xtable.getTable(), context)
				: new ExtendedTextCellEditor(xtable.getTable());
			xtable.getViewer().setCellEditors(new CellEditor[]{null, cellEditor});
		}
		update();
		return xtable.getTable();
	}
	
	public Control getControl() {
		return xtable.getTable();
	}
	
	public void stopEditing() {
		if(attributes != null && xtable.getViewer() != null && xtable.getViewer().isCellEditorActive()) {
			CellEditor editor = xtable.getViewer().getCellEditors()[1];
			if(editor instanceof XCellEditor) {
				((XCellEditor)editor).applyEditorValue();
			} else if(editor instanceof ExtendedCellEditorProvider.StoppableCellEditor) {
				((ExtendedCellEditorProvider.StoppableCellEditor)editor).stopEditing();
			}
		}
	}
	
	public void update() {
		if(xtable.getTable() == null) return;
		xtable.update();
	}

	public void dispose() {
		if (xtable!=null) xtable.dispose();
		xtable = null;
	}
	
	class XTableProviderImpl implements XTableProvider {

		public int getColumnCount() {
			return 2;
		}

		public int getRowCount() {
			return (attributes == null) ? 0 : attributes.getAttributes().length;
		}

		public String getColumnName(int c) {
			return (c == 0) ? "name" : "value";
		}

		public String getValueAt(int r, int c) {
			String name = attributes.getAttributes()[r];
			return (c == 0) ? name : "" + attributes.getAttributeValue(name);
		}

		public Object getDataAt(int r) {
			if(attributes == null) return null;
			AttributeWrapper w = new AttributeWrapper();
			w.attributes = attributes;
			w.name = attributes.getAttributes()[r];
			w.value = attributes.getAttributeValue(w.name);
			return w;
		}
		
		public Color getColor(int r) {
			if(attributes == null) return DISABLED_COLOR;
			String name = attributes.getAttributes()[r];
			return (attributes.isEditableAttribute(name) && !readOnly) ? DEFAULT_COLOR : DISABLED_COLOR;
		}
		
		public int getWidthHint(int c) {
			return (c == 0) ? 10 : 20;
		}

		public void dispose() {
			attributes = null;
		}
	}

}

class CellModifierImpl implements ICellModifier {

	public boolean canModify(Object element, String property) {
		if(!"value".equals(property)) return false;
		AttributeWrapper v = (AttributeWrapper)element;
		return v != null && v.attributes.isEditableAttribute(v.name);
	}

	public Object getValue(Object element, String property) {
		return element;
	}

	public void modify(Object element, String property, Object value) {
		AttributeWrapper v = (AttributeWrapper)value;
		if(v == null || v.value == null) return;
		String stringValue = v.value;
		String old = v.attributes.getAttributeValue(v.name);
		if("".equals(stringValue)) {
			if(old == null || old.length() == 0) return;
		} else {
			if(old != null && old.equals(stringValue)) return;
		}
		v.attributes.setAttributeValue(v.name, stringValue);
		TableItem item = (TableItem)element;
		if (item!=null && !item.isDisposed()) {
			item.setData(v);
			item.setText(1, stringValue);
		}
	}

}

class ExtendedTextCellEditor extends TextCellEditor {
	AttributeWrapper wrapper;
	
	protected Object doGetValue() {
		if(wrapper != null) {
			wrapper.value = (String)super.doGetValue();
		}
		return wrapper;
	}

	public ExtendedTextCellEditor(Composite parent) {
		super(parent);
	}
	protected void doSetValue(Object value) {
		wrapper = (AttributeWrapper)value;
		super.doSetValue(wrapper == null ? "" : wrapper.attributes.getAttributeValue(wrapper.name));
		setValueValid(true);
	}
	
}
