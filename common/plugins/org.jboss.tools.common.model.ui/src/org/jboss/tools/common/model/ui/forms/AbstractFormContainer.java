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

import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public abstract class AbstractFormContainer extends AbstractForm implements IFormContainer {
	
	protected ArrayList<IForm> forms;

	public AbstractFormContainer() {
		forms = new ArrayList<IForm>(3);
	}

	public abstract Control createControl(Composite parent, IWidgetSettings factory);
	public abstract Control getControl();
	
	public boolean addForm(IForm form) {
		form.setEnabled(isEnabled()); 
		return forms.add(form);
	}

	public void addForm(int index, IForm form) {
		form.setEnabled(isEnabled());
		forms.add(index, form);
	}

	public boolean removeForm(IForm form) {
		return forms.remove(form);
	}

	public IForm removeForm(int index) {
		return (IForm)forms.remove(index);
	}

	public IForm get(int index) {
		return (IForm)forms.get(index);
	}

	public boolean contains(IForm form) {
		return forms.contains(form);
	}

	public boolean equals(IFormCollection formCollection) {
		return forms.equals(formCollection);
	}

	public int size() {
		return forms.size();
	}

	public Iterator iterator() {
		return forms.iterator();
	}

	public void clear() {
		forms.clear();
	}

	public void setParent(IFormContainer container) {
		super.setParent(container);
		Iterator i = forms.iterator();
		IForm form;
		while (i.hasNext()) {
			form = (IForm)i.next();
			form.setParent(this);
		}
	}

	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		if (forms!=null && forms.size()>0) {
			Iterator i = forms.iterator();
			while(i.hasNext()) ((IForm)i.next()).setEnabled(enabled);
		}
	}
}
