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
package org.jboss.tools.common.meta.action.impl.handlers;

import java.util.*;
import org.jboss.tools.common.meta.action.XAction;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.engines.impl.EnginesLoader;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;

public class ReplaceWithNewHandler extends DefaultCreateHandler {

	public void executeHandler(XModelObject object, Properties prop) throws Exception {
		if(!isEnabled(object) || data == null || data.length == 0) return;
		String entity = getEntityName();
		Properties p = extractProperties(data[0]);
		setOtherProperties(object, p);
		XModelObject c = XModelObjectLoaderUtil.createValidObject(object.getModel(), entity, p);
		c = modifyCreatedObject(c);
		XModelObject oc = findExistingChild(object, action);
		if(oc != null) {
			if(oc.getModelEntity() == c.getModelEntity()) {
				EnginesLoader.merge(oc, c, true);
			} else {
				DefaultRemoveHandler.removeFromParent(oc);
				addCreatedObject(object, c, p);
			}
		} else {
			addCreatedObject(object, c, p);
		}
	}
    
	public boolean getSignificantFlag(XModelObject object) {
		return (findExistingChild(object, action) != null);
	}
	
	public static XModelObject findExistingChild(XModelObject object, XAction action) {
		XModelObject child = null;
		String childEntity = action.getProperty("childEntity");
		if(childEntity != null) {
			XModelObject[] os = object.getChildren(childEntity);
			if(os.length > 0) child = os[0];
		} else {
			String childName = action.getProperty("child");
			child = object.getChildByPath(childName);
		}
		return child;
	}


}
