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
package org.jboss.tools.common.model.ui.action.sample;

import java.util.HashMap;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;

//import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.actions.IActionProvider;

public class SampleActionProvider implements IActionProvider {
	
	private HashMap<String,IAction> actions = new HashMap<String,IAction>();
	
	public SampleActionProvider() {
	}
	
	protected void initialize(Object model) {
//		XModelObject xmo = (XModelObject)model;
		/*
		this.actions.put(ADD_ACTION, new XActionWrapper(xmo, "CreateActions.Create"));
		this.actions.put(REMOVE_ACTION, new XActionWrapper(xmo, "DeleteActions.Delete"));
		this.actions.put(EDIT_ACTION, new XActionWrapper(xmo, "EditActions.Edit"));
		this.actions.put(UP_ACTION, new XActionWrapper(xmo, "UP_ACTION"));
		this.actions.put(DOWN_ACTION, new XActionWrapper(xmo, "DOWN_ACTION"));
		this.actions.put(DUBLE_CLICK__ACTION, new XActionWrapper(xmo, "DOWN_ACTION"));
		*/
	}

	public IAction getAction(String actionName) {
		return (IAction)actions.get(actionName);
	}

	public IAction[] getActions() {
		String[] keys = (String[])actions.keySet().toArray(new String[actions.keySet().size()]);
		IAction[] result = new IAction[keys.length];
		for (int i=0;i<keys.length;++i) result[i] = (IAction)actions.get(keys[i]);
		return result;
	}

	public void update(ISelection selection) {
	}

}
