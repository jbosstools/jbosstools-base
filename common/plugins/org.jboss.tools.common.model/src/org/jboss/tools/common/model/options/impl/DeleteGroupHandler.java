/*******************************************************************************
 * Copyright (c) 2007 - 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.options.impl;

import org.jboss.tools.common.meta.action.impl.handlers.DefaultRemoveHandler;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.XModelObject;

public class DeleteGroupHandler extends DefaultRemoveHandler {
	
	public DeleteGroupHandler() {}

	public boolean isEnabled(XModelObject object) {
    	if(!super.isEnabled(object)) return false;
    	if(!(object instanceof SharableElementImpl)) return false;
    	SharableElementImpl e = (SharableElementImpl)object;
    	String name = e.getAttributeValue(XModelObjectConstants.ATTR_NAME);
    	if(!(e.getParent() instanceof SharableElementImpl)) return false;
    	SharableElementImpl p = (SharableElementImpl)e.getParent();
    	return p.canRemoveChild(name);
	}

}
