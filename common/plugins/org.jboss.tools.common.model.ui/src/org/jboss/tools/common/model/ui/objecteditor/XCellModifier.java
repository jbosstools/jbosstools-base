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

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.*;

public class XCellModifier implements ICellModifier {

	public boolean canModify(Object element, String property) {
		if(!"value".equals(property)) return false;
		XAttributeInfo v = (XAttributeInfo)element;
		return v != null && v.isEditable();
	}

	public Object getValue(Object element, String property) {
		return element;
	}

	public void modify(Object element, String property, Object value) {
		XAttributeInfo v = (XAttributeInfo)value;
		if(v != null) v.commit();
		TableItem item = (TableItem)element;
		if (item!=null && !item.isDisposed()) {
			item.setText(1, XTable.toVisualValue(v.getValue()));
		}
	}
	
}
