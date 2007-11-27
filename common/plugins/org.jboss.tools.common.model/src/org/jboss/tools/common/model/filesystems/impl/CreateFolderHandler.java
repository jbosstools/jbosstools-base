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
package org.jboss.tools.common.model.filesystems.impl;

import java.util.*;
import org.jboss.tools.common.meta.action.impl.handlers.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;

public class CreateFolderHandler extends DefaultCreateHandler {

	public void executeHandler(XModelObject object, Properties prop) throws Exception {
		if(!isEnabled(object)) return;
		String entity = data[0].getModelEntity().getName();
		Properties p = extractProperties(data[0]);
		XModelObject c = XModelObjectLoaderUtil.createValidObject(object.getModel(), entity, p);
		addCreatedObject(object, c, p);
		if(c instanceof FolderImpl) ((FolderImpl)c).save();
	}
	
}
