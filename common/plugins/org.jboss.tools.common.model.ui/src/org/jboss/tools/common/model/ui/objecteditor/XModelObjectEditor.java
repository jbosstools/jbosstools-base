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

import java.util.ArrayList;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class XModelObjectEditor {
	static Color DEFAULT_COLOR = new Color(null, 0, 0, 0);
	static Color DISABLED_COLOR = new Color(null, 127, 127, 127);
	static Image EMPTY = EclipseResourceUtil.getImage("images/actions/empty.gif");
	protected XTable xtable = new XTable();
	protected XModelObject object = null;
	protected XAttribute[] attributes = new XAttribute[0];
	protected XCellModifier modifier = new XCellModifier();
	protected boolean viewMode = false; 
	
	public XModelObjectEditor() {
		xtable.setTableProvider(new XTableProviderImpl());
	}
	
	public void dispose() {
		if (xtable!=null) xtable.dispose();
		xtable = null;
	}
	
	public void setModelObject(XModelObject object) {
		if(this.object == object) return;
		stopEditing();		
		this.object = object;
		if(xtable.getTable() == null || xtable.getTable().isDisposed()) return; 
		xtable.getTable().setSelection(-1);
		updateTimeStamp = -2;
		update();
	}
	
	public void setViewMode(boolean b) {
		viewMode = b;
	}

	public Control createControl(Composite parent) {
		return createControl(parent, xtable.style);
	}
	
	public Control createControl(Composite parent, int style) {
		updateTimeStamp = -1;
		xtable.createControl(parent, style);
		xtable.getViewer().setColumnProperties(new String[]{"name", "value"});
		xtable.getViewer().setCellModifier(modifier);
		if(!viewMode) {
			xtable.getViewer().setCellEditors(new CellEditor[]{null, new XCellEditor(xtable.getTable())});
		}
		update();
		return xtable.getTable();
	}
	
	public Control getControl() {
		return xtable.getTable();
	}
	
	protected void loadAttributes() {
		ArrayList<XAttribute> list = new ArrayList<XAttribute>();
		XAttribute[] as = (object == null) ? new XAttribute[0] : object.getModelEntity().getAttributes();
		for (int i = 0; i < as.length; i++) 
		  if(as[i].isVisible()) list.add(as[i]);
		attributes = list.toArray(new XAttribute[0]);		
	}
	public void stopEditing() {
		if(object != null && xtable.getViewer() != null && xtable.getViewer().isCellEditorActive()) {
			CellEditor editor = xtable.getViewer().getCellEditors()[1];
			if(editor instanceof XCellEditor) {
				((XCellEditor)editor).applyEditorValue();
			}
		}
	}
	
	long updateTimeStamp = -1;
	
	public void update() {
		if(xtable.getTable() == null) return;
		long ts = (object == null) ? -1 : object.getTimeStamp();
		if(updateTimeStamp == ts) return;
		updateTimeStamp = ts;
		loadAttributes();
		xtable.update();
	}

	class XTableProviderImpl implements XTableProvider {

		public int getColumnCount() {
			return 2;
		}

		public int getRowCount() {
			return attributes.length;
		}

		public String getColumnName(int c) {
			return (c == 0) ? "name" : "value";
		}

		public String getValueAt(int r, int c) {
			if(c == 0) {
				String labelText = WizardKeys.getAttributeDisplayName(attributes[r], true);
				labelText = (labelText == null) ? attributes[r].getName() : labelText.toLowerCase();
				return labelText;

			}
			return "" + object.getAttributeValue(attributes[r].getName());
		}

		public Object getDataAt(int r) {
			return new XAttributeInfo(object, attributes[r].getName());
		}
		
		public Color getColor(int r) {
			XAttributeInfo data = (XAttributeInfo)getDataAt(r);
			return (data.isEditable() && !viewMode) ? DEFAULT_COLOR : DISABLED_COLOR;
		}
		
		public int getWidthHint(int c) {
			return (c == 0) ? 10 : 20;
		}

		public void dispose() {
			object = null;
		}
	}
}
