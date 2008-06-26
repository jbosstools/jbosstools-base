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

import org.jboss.tools.common.model.ui.action.filter.IModelObjectActionFilter;
import org.eclipse.ui.IActionFilter;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.adapter.IModelObjectAdapter;

public class XModelObjectActionFilter implements IActionFilter, IModelObjectAdapter {
	XModelObject object;

	public XModelObjectActionFilter() {}

	public void setModelObject(XModelObject object) {
		this.object = object;
	}

	public boolean testAttribute(Object target, String name, String value) {
		try {
			IModelObjectActionFilter f = (IModelObjectActionFilter)getClass().getClassLoader().loadClass(name).newInstance();
			return f.isEnabled(object, value);
		} catch (Exception e) {
			//debug
			return false;
		}
	}
	

}
