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
package org.jboss.tools.common.editor.form;

import java.util.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.AbstractTableHelper;
import org.jboss.tools.common.model.ui.objecteditor.*;

public class DefaultChildrenForm implements IForm {
	SashForm composite = null;
	PropertyForm properties = new PropertyForm();
	XModelObject selected;
	AdjustableChildrenEditor editor = new AdjustableChildrenEditor();
	
	public void dispose() {
		if (editor!=null) editor.dispose();
		editor = null;
		if (properties!=null) properties.dispose();
		properties = null;
	}
	
	public void setInput(Object object) {
		selected = (XModelObject)object;
		String[] h = editor.getHelper().getHeader();
		editor.getHelper().setEntity(selected.getModelEntity(), 0);
		if(h != editor.getHelper().getHeader()) {
			if(composite != null) {
				editor.getControl().dispose();
				editor.createControl(composite);
				composite.setWeights(new int[]{15, 30});
				composite.update();
				composite.layout();
			}
		}
		editor.setObject(selected);
		editor.update();
		properties.setInput(selected);
	}

	public Control createControl(Composite parent) {
		composite = new SashForm(parent, SWT.VERTICAL);
		properties.createControl(composite);
		editor.createControl(composite);
		composite.setWeights(new int[]{15, 30});
		return composite;
	}

	public Control getControl() {
		return composite;
	}

	public void update() {
		editor.update();
		properties.update();
	}
    
}

class AdjustableChildrenEditor extends XChildrenEditor {
	protected AbstractTableHelper createHelper() {
		return new Helper();
	}
	
	public Helper getHelper() {
		return (Helper)helper;		
	}

	protected void edit() {
		XModelObject o = helper.getModelObject(xtable.getSelectionIndex());
		if(o == null) return;
		XActionList l = (XActionList)o.getModelEntity().getActionList();
		if(l.getAction("Edit") != null) {
			callAction(o, "Edit");
		} else if(l.getAction("EditActions.Edit") != null) {
			callAction(o, "EditActions.Edit");
		} else {
			callAction(o, "Properties.Properties");
		}
	}
	
	protected void add() {
		String s = getAddActionPath();
		if(s != null) super.add();
	}
	
	protected String getAddActionPath() {
		XModelObject o = helper.getModelObject();
		XActionList l = (XActionList)o.getModelEntity().getActionList().getItem("CreateActions");
		XActionItem[] is = l.getActionItems();
		if(is.length != 1 || !(is[0] instanceof XAction)) return null;
		return "CreateActions" + "." + is[0].getName();
	}

}
	
class Helper extends AbstractTableHelper {
	String[] headerNames = new String[0];
	public void setEntity(XModelEntity entity, int child) {
		String cen = entity.getChildren()[child].getName();
		XModelEntity ce = entity.getMetaModel().getEntity(cen);
		if(ce == null) return;
		XAttribute[] as = ce.getAttributes();
		ArrayList<String> l = new ArrayList<String>();
		for (int i = 0; i < as.length; i++) {
			if(!as[i].isVisible()) continue;
			if("element type".equals(as[i].getName())) continue;
			l.add(as[i].getName());
			if(l.size() >= 4) break;
		}
		String[] h = l.toArray(new String[0]);
		if(!same(h)) headerNames = h;
	}
	
	private boolean same(String[] h) {
		if(headerNames.length != h.length) return false;
		for (int i = 0; i < h.length; i++) if(!headerNames[i].equals(h[i])) return false;
		return true;		
	}

	public String[] getHeader() {
		return headerNames;
	}
	
}
