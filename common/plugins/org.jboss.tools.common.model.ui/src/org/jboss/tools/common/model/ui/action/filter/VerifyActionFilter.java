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
package org.jboss.tools.common.model.ui.action.filter;

import org.jboss.tools.common.model.ui.dnd.DnDUtil;

import org.jboss.tools.common.model.XModelObject;

public class VerifyActionFilter implements IModelObjectActionFilter {
	static String path1 = "VerifyActions.StaticActions.VerifyAll";
	static String path2 = "ProcessVerifyActions.StaticActions.VerifyAll";
	
	public boolean isEnabled(XModelObject object, String info) {
		return (object != null) && (DnDUtil.getEnabledAction(object, null, getActionPath(object)) != null);
	}

	protected String getActionPath(XModelObject object) {
		return (object.getModelEntity().getActionList().getAction(path2) != null) ? path2 : path1;
	}
	
}
