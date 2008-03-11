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

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.loaders.*;

public class FileSystemFolderLoader implements XObjectLoader {

    public FileSystemFolderLoader() {}

    public void load(XModelObject object) {}

    public boolean update(XModelObject object) {
        FolderLoader folder = (FolderLoader)object;
        if(folder.isRemoved() && !isProtectedFileSystem(object)) {
        	object.getParent().setModified(true);
        	object.removeFromParent();
        }
        return folder.update();
    }
    
    private boolean isProtectedFileSystem(XModelObject object) {
    	String name = object.getAttributeValue("name");
    	if("WEB-ROOT".equals(name) || "WEB-INF".equals(name)) return true;
    	return false;
    }

    public boolean save(XModelObject object) {
        FolderLoader folder = (FolderLoader)object;
        return folder.save();
    }

} 