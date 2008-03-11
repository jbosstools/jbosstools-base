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
package org.jboss.tools.common.gef.action;

import java.util.*;
import org.eclipse.draw2d.PositionConstants;
import org.eclipse.gef.ui.actions.*;
import org.eclipse.jface.action.*;
import org.eclipse.ui.*;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.RetargetAction;

public class ActionRegistrySupport {
	private IWorkbenchPage page;
	private ActionRegistry registry = new ActionRegistry();
	private List<IAction> retargetActions = new ArrayList<IAction>();
	private List<String> globalActionKeys = new ArrayList<String>();
	
	public void setPage(IWorkbenchPage page) {
		this.page = page;
	}
	
	public IWorkbenchPage getPage() {
		return page;
	}

	public void addGlobalActionKey(String key) {
		globalActionKeys.add(key);
	}
	
	public Iterator getGlobalActionKeys() {
		return globalActionKeys.iterator();   
	}
	
	public ActionRegistry getActionRegistry() {
		return registry;
	}
	
	public void addAction(IAction action) {
		getActionRegistry().registerAction(action);
	}
	
	public void addRetargetAction(IAction action) {
		addAction(action);
		retargetActions.add(action);
		if(action instanceof RetargetAction) {
			getPage().addPartListener((RetargetAction)action);
		}
		addGlobalActionKey(action.getId());
	}

	public IAction getAction(String id) {
		return getActionRegistry().getAction(id);
	}

	public void buildGEFActions() {
		addRetargetAction(new PrintRetargetAction());
		addRetargetAction(ActionFactory.DELETE.create(getPage().getWorkbenchWindow()));
		addRetargetAction(ActionFactory.COPY.create(getPage().getWorkbenchWindow()));
		addRetargetAction(ActionFactory.PASTE.create(getPage().getWorkbenchWindow()));
	
		addRetargetAction(new AlignmentRetargetAction(PositionConstants.LEFT));
		addRetargetAction(new AlignmentRetargetAction(PositionConstants.CENTER));
		addRetargetAction(new AlignmentRetargetAction(PositionConstants.RIGHT));
		addRetargetAction(new AlignmentRetargetAction(PositionConstants.TOP));
		addRetargetAction(new AlignmentRetargetAction(PositionConstants.MIDDLE));
		addRetargetAction(new AlignmentRetargetAction(PositionConstants.BOTTOM));
	
		addRetargetAction(new ZoomInRetargetAction());
		addRetargetAction(new ZoomOutRetargetAction());
	}

	public void contributeGEFToToolBar(IToolBarManager tbm) {
		tbm.add(getAction("Print_Diagram"));
		tbm.add(new Separator());
		tbm.add(getAction(GEFActionConstants.ALIGN_LEFT));
		tbm.add(getAction(GEFActionConstants.ALIGN_CENTER));
		tbm.add(getAction(GEFActionConstants.ALIGN_RIGHT));
		tbm.add(new Separator());
		tbm.add(getAction(GEFActionConstants.ALIGN_TOP));
		tbm.add(getAction(GEFActionConstants.ALIGN_MIDDLE));
		tbm.add(getAction(GEFActionConstants.ALIGN_BOTTOM));
		
		tbm.add(new Separator());
		tbm.add(new ZoomComboContributionItem(getPage()));
	}

	public void declareGlobalActionKeys() {
		addGlobalActionKey(ActionFactory.PRINT.getId());
		addGlobalActionKey(ActionFactory.COPY.getId());
		addGlobalActionKey(ActionFactory.PASTE.getId());
		addGlobalActionKey(ActionFactory.CUT.getId());
		addGlobalActionKey(ActionFactory.DELETE.getId());
	}	
	
	public void dispose() {
		if(retargetActions == null) return;
		for (int i = 0; i < retargetActions.size(); i++) {
			RetargetAction action = (RetargetAction)retargetActions.get(i);
			getPage().removePartListener(action);
		}
		registry.dispose();
		retargetActions = null;
		registry = null;
	}

}
