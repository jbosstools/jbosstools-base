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
package org.jboss.tools.common.model.ui.action;

import java.util.*;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.actions.ActionContext;
import org.jboss.tools.common.meta.action.XActionList;
import org.jboss.tools.common.model.XModelObject;

public class ModelContributionManager extends MenuManager {
	ActionContext context;
	ISelection selection;
	Shell shell;
	
	public ModelContributionManager(Shell shell) {
		super("JBoss Tools");
		this.shell = shell;
	}
	
	public void setContext(ActionContext context) {
		this.context = context;
	}

	public void setSelection(ISelection s) {
		selection = s;
	}
	
	protected void update(boolean force, boolean recursive) {
		if (isDirty() || force) {
			if (getMenu() != null && !getMenu().isDisposed()) {
				Menu menu = getMenu();
				MenuItem[] is = menu.getItems();
				for (int i = 0; i < is.length; i++) is[i].dispose();
				XModelObject[] os = getSelectedModelObjects();
				if(os.length == 0) return;
				XModelObject o = os[0];
				if(os.length == 1) os = null;
				XModelObjectActionList l = new XModelObjectActionList(getActionList(o), o, os, new Object[]{o});
				l.setShell(shell);
				l.createMenu(menu);
				l.removeLastSeparator(menu);
			}
		}
	}

	public XActionList getActionList(XModelObject o) {
		return o.getModelEntity().getActionList();
	}

	public XModelObject[] getSelectedModelObjects() {
		ISelection s = context != null ? context.getSelection() : selection;
		if(s.isEmpty() || !(s instanceof StructuredSelection)) return new XModelObject[0];
		StructuredSelection ss = (StructuredSelection)s;
		Iterator it = ss.iterator();
		ArrayList<XModelObject> l = new ArrayList<XModelObject>();
		while(it.hasNext()) {
			IAdaptable a = (IAdaptable)it.next();
			XModelObject o = (XModelObject)a.getAdapter(XModelObject.class);
			if(o != null) l.add(o);
		}
		return l.toArray(new XModelObject[0]); 
	}
	
}
