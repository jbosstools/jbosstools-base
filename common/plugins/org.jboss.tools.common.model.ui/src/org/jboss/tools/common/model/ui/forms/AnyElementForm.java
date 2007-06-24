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
package org.jboss.tools.common.model.ui.forms;

import java.util.*;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.common.model.ui.action.CommandBar;
import org.jboss.tools.common.model.ui.action.CommandBarListener;
import org.jboss.tools.common.model.ui.objecteditor.XChildrenEditor;
import org.jboss.tools.common.model.ui.objecteditor.XTable;
import org.jboss.tools.common.model.ui.objecteditor.XTableProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.impl.AnyElementObjectImpl;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.forms.ExpandableForm;
import org.jboss.tools.common.model.ui.widgets.*;

public class AnyElementForm extends ExpandableForm {
	private XModelObject xmo;
	private XTable tableEditor = new XTable();
	TableProviderImpl tableProvider = new TableProviderImpl();
	CommandBar bar = new CommandBar();
	private IWidgetSettings settings = new WhiteSettings();

	public AnyElementForm() {
		this.setCollapsable(Boolean.TRUE.booleanValue());
		tableEditor.setTableProvider(tableProvider);
		bar.setCommands(new String[]{XChildrenEditor.ADD, XChildrenEditor.EDIT, XChildrenEditor.DELETE});
		bar.addCommandBarListener(new CommandBarListenerImpl());
		bar.getLayout().direction = SWT.VERTICAL;
		bar.setWidgetSettings(settings);
		bar.getLayout().buttonWidth = 80;
	}

	public void dispose() {
		super.dispose();
		if (tableEditor != null) {
			tableEditor.dispose();
			tableEditor = null;
		}
		if(tableProvider != null) {
			tableProvider.dispose();
			tableProvider = null;
		}
	}

	protected Control createClientArea(Composite parent, IWidgetSettings settings) {
		Composite composite = new Composite(parent, SWT.NONE);
			composite.setBackgroundMode(SWT.INHERIT_DEFAULT);
		settings.setupControl(composite);
		GridLayout layout = new GridLayout(2, Boolean.FALSE.booleanValue());

		layout.horizontalSpacing = 5;
		layout.verticalSpacing = 5;
		layout.marginHeight = 5;
		layout.marginWidth = 5;
		composite.setLayout(layout);
		if(xmo == null) return composite;

		String description = "";
		if(description != null && description.length() > 0) {
			Label label = new Label(composite, SWT.WRAP);
			settings.setupControl(label);
			label.setText(description);
			GridData gd = new GridData();
			gd.horizontalSpan = 2;
			label.setLayoutData(gd);
		}
		
		Composite c = new Composite(composite, SWT.NONE);
		settings.setupControl(c);
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = 2;
		c.setLayoutData(gd);
		GridLayout layout2 = new GridLayout(2,false);
		layout2.marginHeight = 0;
		layout2.marginWidth = 0;
		c.setLayout(layout2);
		
		Control control = tableEditor.createControl(c);
		gd = new GridData(GridData.FILL_BOTH);
		control.setLayoutData(gd);
		
		control = bar.createControl(c);
		gd = new GridData(GridData.FILL_VERTICAL);
		control.setLayoutData(gd);

		tableEditor.update();
		tableEditor.getViewer().setColumnProperties(tableProvider.header);
		tableEditor.getViewer().setCellEditors(new CellEditor[]{new TextCellEditor(tableEditor.getTable()), new TextCellEditor(tableEditor.getTable())});
		tableEditor.getViewer().setCellModifier(new CellModifier());
		tableEditor.getViewer().addSelectionChangedListener(new SCL());
		updateCommandBar();
		
		return composite;
	}

/*
	private void putFieldEditorInToComposit(Composite composite, IPropertyEditor propertyEditor) {
		if (propertyEditor != null) {
			propertyEditor.getFieldEditor(composite).fillIntoGrid(composite, 2);
//			support.registerFieldEditor(propertyEditor.getAttributeName(), propertyEditor.getFieldEditor(composite));
		}
	}
*/

	public void initialize(Object model) {
		String heading = "Attributes";
		this.setHeadingText(heading);
		if(!(model instanceof XModelObject)) {
			Status s = new Status(Status.OK, ModelUIPlugin.PLUGIN_ID, Status.OK, "Error to create form " + heading + ". Model object cannot be null.", new Exception());
			ModelUIPlugin.log(s);
			return;
		}
		this.xmo = (XModelObject)model;
		updateAttributes();
	}

	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
	}
	
	private long timeStamp = -1;
	
	public void update() {
		long ts = (xmo == null) ? -1 : xmo.getTimeStamp();
		if(ts == timeStamp) return;
		timeStamp = ts;
		updateAttributes();
		tableEditor.update();
	}
	
	void updateAttributes() {
		if(tableProvider == null) return;
		if(xmo == null) {
			tableProvider.attributes = null;
		} else {
			String attrs = xmo.getAttributeValue("attributes");
			StringTokenizer st = new StringTokenizer(attrs, ";");
			int length = st.countTokens();
			String[][] as = new String[length][2];
			for (int i = 0; i < length; i++) {
				String t = st.nextToken();
				int k = t.indexOf('=');
				String n = k < 0 ? "" : t.substring(0, k);
				String v = t.substring(k + 1);
				as[i][0] = n;
				as[i][1] = v;
			}
			tableProvider.attributes = as;
		}
	}
	
	class TableProviderImpl implements XTableProvider {
		String[] header = new String[]{"name", "value"};
		String[][] attributes = null;

		public int getColumnCount() {
			return 2;
		}
		public int getRowCount() {
			return attributes == null ? 0 : attributes.length;
		}
		public String getColumnName(int c) {
			return header[c];
		}
		public String getValueAt(int r, int c) {
			return attributes == null ? null : attributes[r][c];
		}
		public Object getDataAt(int r) {
			return new Integer(r);
		}
		public Color getColor(int r) {
			return null;
		}
		public int getWidthHint(int c) {
			return 10;
		}
		public void dispose() {
		}
	}
	
	class CellModifier implements ICellModifier {

		public boolean canModify(Object element, String property) {
			if(!"value".equals(property)) return false;
			return xmo != null && xmo.isObjectEditable() && getColumn(property) >= 0;
		}

		public Object getValue(Object element, String property) {
			int r = ((Integer)element).intValue();
			int c = getColumn(property);
			return (c < 0) ? "" : tableProvider.getValueAt(r, c);
		}
		
		int getColumn(String property) {
			for (int c = 0; c < tableProvider.header.length; c++) {
				if(property.equals(tableProvider.header[c])) return c;
			}
			return -1;
		}

		public void modify(Object element, String property, Object value) {
			int r = ((Integer)((TableItem)element).getData()).intValue();
			int c = getColumn(property);
			if(c < 0) return;
			String oldValue = tableProvider.getValueAt(r, c);
			if(oldValue != null && oldValue.equals(value)) return;
			tableProvider.attributes[r][c] = "" + value;
			commitAttributes();
		}
		
	}
	
	void commitAttributes() {
		if(xmo == null || tableProvider == null || tableProvider.attributes == null) return;
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < tableProvider.attributes.length; i++) {
			String n = tableProvider.attributes[i][0].trim();
			if(n.length() == 0) continue;
			String v = tableProvider.attributes[i][1].trim();
			if(sb.length() > 0) sb.append(AnyElementObjectImpl.SEPARATOR);
			sb.append(n).append('=').append(v);
		}
		String v = sb.toString();
		xmo.getModel().editObjectAttribute(xmo, "attributes", v);		
	}
	
	class CommandBarListenerImpl implements CommandBarListener {
		public void action(String command) {
			if(XChildrenEditor.ADD.equals(command)) {
				add();
			} else if(XChildrenEditor.EDIT.equals(command)) {
				edit();
			} else if(XChildrenEditor.DELETE.equals(command)) {
				delete();
			}			
		}
	}
	
	void updateCommandBar() {
		boolean enabled = xmo != null && xmo.isObjectEditable();
		int[] is = tableEditor.getTable().getSelectionIndices();
		int selected = (is == null) ? 0 : is.length;
		bar.setEnabled(XChildrenEditor.EDIT, enabled && selected == 1);
		bar.setEnabled(XChildrenEditor.DELETE, enabled && selected > 0);
		bar.setEnabled(XChildrenEditor.ADD, enabled);
	}
	
	class SCL implements ISelectionChangedListener {
		public void selectionChanged(SelectionChangedEvent event) {
			updateCommandBar();
		}
	}
	
	void add() {
		XActionInvoker.invoke("CreateActions.CreateAttribute", xmo, new Properties());
	}
	
	void edit() {
		if(tableEditor.getTable() == null || tableEditor.getTable().isDisposed()) return;
		int i = tableEditor.getTable().getSelectionIndex();
		if(i < 0) return;
		String name = tableProvider.getValueAt(i, 0);
		Properties p = new Properties();
		p.setProperty("name", name);
		try {
			XActionInvoker.invoke("AnyElementNew", "EditActions.EditAttribute", xmo, p);
		} catch (Exception e) {}
	}
	
	void delete() {
		if(tableEditor.getTable() == null || tableEditor.getTable().isDisposed()) return;
		int[] is = tableEditor.getTable().getSelectionIndices();
		if(is == null || is.length == 0) return;
		String name = tableProvider.getValueAt(is[0], 0);
		ServiceDialog d = xmo.getModel().getService();
		String message = null;
		if(is.length == 1) {
			message = "Delete attribute " + name + "?";
		} else {
			message = "Delete " + is.length + " attributes?";
		}
		int q = d.showDialog("Delete", message, new String[]{"OK", "Cancel"}, null, ServiceDialog.QUESTION);
		if(q != 0) return;
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < tableProvider.attributes.length; i++) {
			boolean a = true;
			for (int j = 0; j < is.length && a; j++) if(is[j] == i) a = false;
			if(!a) continue;
			String n = tableProvider.attributes[i][0].trim();
			if(n.length() == 0) continue;
			String v = tableProvider.attributes[i][1].trim();
			if(sb.length() > 0) sb.append(AnyElementObjectImpl.SEPARATOR);
			sb.append(n).append('=').append(v);
		}
		String v = sb.toString();
		xmo.getModel().editObjectAttribute(xmo, "attributes", v);		
	}

}
