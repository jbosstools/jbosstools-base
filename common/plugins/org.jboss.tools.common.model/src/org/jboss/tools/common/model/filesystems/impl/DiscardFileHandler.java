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
import org.jboss.tools.common.meta.action.impl.AbstractHandler;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;

public class DiscardFileHandler extends AbstractHandler {

	public boolean isEnabled(XModelObject object) {
		return object != null && object.isActive() && object.getFileType() == XFileObject.FILE
			   && object.isModified();
	}

	public void executeHandler(XModelObject object, Properties p) throws Exception {
		if(!isEnabled(object)) return;
		XModelObject po = object.getParent();
		if(po instanceof FolderImpl) {
			FolderImpl parent = (FolderImpl)po;
			parent.discardChildFile(object);
		} else if(po instanceof JarFolderImpl) {
//			JarFolderImpl parent = (JarFolderImpl)po;
			object.setModified(false);
			XModelObjectLoaderUtil.updateModifiedOnSave(object);
		}
		if(p != null) p.setProperty("done", "true");
	}
	
	public boolean getSignificantFlag(XModelObject object) {
		return true;
	}

}
