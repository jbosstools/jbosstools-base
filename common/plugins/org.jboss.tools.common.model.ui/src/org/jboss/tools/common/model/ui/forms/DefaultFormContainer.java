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

import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IMemento;

import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;

public class DefaultFormContainer extends AbstractFormContainer {
	
	private Composite composite;
	private IWidgetSettings settings;

	public DefaultFormContainer() {
		super();
	}

	public DefaultFormContainer(IForm form) {
		super();
		addForm(form);
	}

	public Control createControl(Composite parent, IWidgetSettings settings) {
		this.settings = settings;
		composite = new Composite(parent, SWT.NONE);
		settings.setupControl(composite);
		
		//composite.setBackground(new Color(null, 255, 0 ,0));
		composite.setLayout(getLayout());
		composite.setLayoutData(getLayoutData());
		// create client area
		for (int i=0;i<this.size();++i) {
			get(i).createControl(composite, settings);
		}
		//reflow();
		return composite;
	}

	public Control getControl() {
		return composite;
	}

	// -------------------------------------

	public boolean addForm(IForm form) {
		if (composite != null) {
			Control control = form.getControl();
			if (control == null) {
				control = form.createControl(this.composite, this.settings);
			} else {
				control.setParent(this.composite);
			}
			GridData gd;
			gd = new GridData(GridData.FILL_BOTH);
			control.setLayoutData(gd);
			reflow();
		}
		form.setParent(this);
		form.setEnabled(isEnabled());
		return forms.add(form);
	}

	protected void reflow() {
		this.composite.setRedraw(false);
		this.composite.getParent().setRedraw(false);
		//control.getParent().getParent().setRedraw(false);
		this.composite.layout(true);
		this.composite.getParent().layout(true);
		//control.getParent().getParent().layout(true);
		this.composite.setRedraw(true);
		this.composite.getParent().setRedraw(true);
		//control.getParent().getParent().setRedraw(true);
	}

	public void addForm(int index, IForm form) {
		//form.setParent(this);
		//forms.add(index, form);
	}

	public boolean removeForm(IForm form) {
		Control control = form.getControl();
		control.setParent(null);
		reflow();
		return forms.remove(form);
	}

	public IForm removeForm(int index) {
		return null;
		//return (IForm)forms.remove(index);
	}

	public void clear() {
		Iterator i = forms.iterator();
		IForm form;
		while (i.hasNext()) {
			form = (IForm)i.next();
			form.dispose();
			form = null; 
		}
		forms.clear();
	}

	// ----------------------
	public void setFocus() {
		// TODO wait for IFormSelectionListener
		// get focused form
	}

	public void dispose() {
		for (int i=0;i<size();++i) get(i).dispose();
		if (composite!=null && !composite.isDisposed())	composite.dispose();
		composite = null;
	}

	public void initialize(Object model) {
		for (int i=0;i<size();++i) get(i).initialize(model);
	}

	public void commitChanges(boolean onSave) {
		for (int i=0;i<size();++i) get(i).commitChanges(onSave);
	}

	public boolean doGlobalAction(String actionId) {
		// TODO wait for IFormSelectionListener
		// get focused form
		// TODO as temp notify all forms
		boolean result = Boolean.FALSE.booleanValue();
		Iterator i = iterator();
		IForm form;
		while (i.hasNext()) {
			form = (IForm)i.next();
			result &= form.doGlobalAction(actionId);
		}
		return result;
	}

	public void expandTo(Object object) {
		for (int i=0;i<size();++i) get(i).expandTo(object);
	}

	public void update() {
		for (int i=0;i<size();++i) get(i).update();
//		if(getParent()!=null) {
//			getParent().update();
//		}
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.forms.IForm#store(org.eclipse.ui.IMemento)
	 */
	public void store(IMemento memento) {
		if (forms!=null && forms.size()>0) {
			Iterator i = forms.iterator();
			while (i.hasNext()) {
				IForm form = (IForm)i.next();
				IMemento formMemento = memento.getChild(form.getHeadingText());
				if (formMemento==null) formMemento = memento.createChild(form.getHeadingText());
				form.store(formMemento);
			}
		}
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.forms.IForm#load(org.eclipse.ui.IMemento)
	 */
	public void load(IMemento memento) {
		if (forms!=null && forms.size()>0) {
			Iterator i = forms.iterator();
			while (i.hasNext()) {
				IForm form = (IForm)i.next();
				IMemento formMemento = memento.getChild(form.getHeadingText());
				if (formMemento==null) formMemento = memento.createChild(form.getHeadingText());
				form.load(formMemento);
			}
		}
	}
}