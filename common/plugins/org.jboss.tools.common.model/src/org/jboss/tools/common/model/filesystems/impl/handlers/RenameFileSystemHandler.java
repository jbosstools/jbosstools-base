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
package org.jboss.tools.common.model.filesystems.impl.handlers;

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.impl.handlers.DefaultEditHandler;

public class RenameFileSystemHandler extends DefaultEditHandler {
	static String RFS = "root file system"; 
	static String SFS = "src file system"; 
	
	public boolean isEnabled(XModelObject object) {
		if(!super.isEnabled(object)) return false;
		String nm = object.getAttributeValue("name");
		return !"WEB-INF".equals(nm);	
	}
	public void executeHandler(XModelObject object, Properties prop) throws Exception {
		if(!isEnabled(object) || data == null) return;
		Properties p = extractProperties(data[0]);
		setOtherProperties(object, p);
		String oldName = object.getAttributeValue("name");
		edit(object, p);
		String newName = p.getProperty("name");
		XModelObject web = object.getModel().getByPath("Web");
		if(web == null) return;
		XModelObject[] ms = web.getChildren();
		for (int i = 0; i < ms.length; i++) {
			String f = ms[i].getAttributeValue(RFS);
			if(oldName.equals(f)) object.getModel().changeObjectAttribute(ms[i], RFS, newName);
			f = ms[i].getAttributeValue(SFS);
			if(oldName.equals(f)) object.getModel().changeObjectAttribute(ms[i], SFS, newName);
		}
	}

}
