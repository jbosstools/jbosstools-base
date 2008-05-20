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
package org.jboss.tools.common.model.ui.attribute.adapter;

import java.util.ArrayList;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;

import org.jboss.tools.common.model.ui.actions.IActionProvider;

/**
 * @author au
 */
public class CompositeActionProvider implements IActionProvider {

    private ArrayList<IAction> actions = new ArrayList<IAction>();
    
    public void addAction(IAction action) {
        this.actions.add(action);
    }
    public IAction getAction(String actionName) {
        return null;
    }

    public IAction[] getActions() {
        return (IAction[])actions.toArray(new IAction[actions.size()]);
    }

	public void update(ISelection selection) {
	}
}
