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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.layout.*;

import org.jboss.tools.common.meta.XChild;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class FormPlace {
	Composite composite = null;
	XModelObject selected = null;
	IForm form = null;
	
	public Control createControl(Composite parent) {
		composite = new Composite(parent, SWT.NONE);
		composite.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_WHITE));
		GridLayout layout = new GridLayout();
		layout.marginHeight = 0;
		layout.marginWidth = 0;
		composite.setLayout(layout);
		if(form != null) {
			form.createControl(composite);
		}
		return composite;
	}

	public Control getControl() {
		return composite;
	}
	
	public void setSelectedObject(XModelObject object) {
		if(selected == object) return;
		selected = object;
		Class c = getFormClassForSelection();
		if(form == null || c == null || form.getClass() != c/* || DefaultChildrenForm.class == c*/) {
			if(form != null) {
				form.dispose();
				form = null;
			} 
			if(c != null) installForm(c);
		} else {
			form.setInput(selected);
		}
	}
	
	private void installForm(Class cls) {
		try {
			form = (IForm)cls.newInstance();
		} catch (Exception e) {
			ModelUIPlugin.log(e);
			return;
		}
		form.setInput(selected);
		if(composite != null && !composite.isDisposed()) {
			Control c = form.createControl(composite);
			c.setLayoutData(new GridData(GridData.FILL_BOTH));
			composite.update();
			composite.layout();
		}
	}

	public void dispose() {
		if(form != null) form.dispose();
		form = null;
		if(composite != null) {
			try { composite.dispose(); } catch (Exception e) {}
			composite = null;
		}
	}
	
	private Class getFormClassForSelection() {
		if(selected == null) return null;
		XChild[] cs = selected.getModelEntity().getChildren();
		int cl = cs.length;
		if(cl == 0) return PropertyForm.class;
		if(cl > 1) return PropertyForm.class;
		if(cl == 1) return DefaultChildrenForm.class;
		return null;
	}
	
	public void update() {
		if(form != null) form.update();
	}
	
}
