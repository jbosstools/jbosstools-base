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
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.XModelObject;

public class ChildrenDeleteHandler extends AbstractHandler {

	public boolean isEnabled(XModelObject object) {
		return (object != null && object.isObjectEditable() && object.getChildren().length > 0);
	}

	public void executeHandler(XModelObject object, Properties p) throws Exception {
		if(!isEnabled(object)) return;
		XModelObject[] cs = object.getChildren();
		XActionInvoker.invoke("DeleteActions.Delete", cs[0], cs, p);
	}

}
