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
package org.jboss.tools.common.model.ui.views.navigator;

import java.util.*;

import org.jboss.tools.common.model.ui.dnd.DnDUtil;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.actions.BaseSelectionListenerAction;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XModelObject;

public class NSelectionAction extends BaseSelectionListenerAction {
	private boolean selectionDirty = true;
	XModelObject target;
	XModelObject[] targets;

	protected NSelectionAction(String text) {
		super(text);
	}

	protected void clearCache() {
		selectionDirty = true;
		target = null;
		targets = null;
	}

	private final void computeResources() {
		if(!selectionDirty) return;
		selectionDirty = false;
		IStructuredSelection selection = getStructuredSelection();
		if(selection == null || selection.isEmpty()) return;
		Object o = selection.getFirstElement();
		if(!(o instanceof XModelObject)) return;
		target = (XModelObject)o;
		List<XModelObject> list = new ArrayList<XModelObject>();
		Iterator it = selection.iterator();
		while(it.hasNext()) {
			o = it.next();
			if(o instanceof XModelObject) list.add((XModelObject)o);
		}
		if(list.size() > 1) targets = list.toArray(new XModelObject[0]);
	}

	protected final boolean updateSelection(IStructuredSelection selection) {
		computeResources();
		return updateSelection(target, targets);
	}

	protected boolean updateSelection(XModelObject target, XModelObject[] targets) {
		return isXActionEnabled();
	}
	
	private boolean isXActionEnabled() {
		return DnDUtil.getEnabledAction(target, targets, getActionPath()) != null;
	}
	
	protected String getActionPath() {
		return null;
	}

	public void run() {
		if(!isXActionEnabled()) return;
		XActionInvoker.invoke(getActionPath(), target, targets, new Properties());
	}
		
}
