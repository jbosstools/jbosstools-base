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
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.util.*;

public class MountFolderHandler extends AbstractHandler {
    private MountFileSystemHandler h = new MountFileSystemHandler();

    public MountFolderHandler() {}

    public boolean isEnabled(XModelObject object) {
        if(object == null || "true".equals(object.get("overlapped"))) return false;
        String entity = object.getModelEntity().getName();
        if("FileFolder".equals(entity)) return true;
        if(object.getFileType() == XFileObject.FILE &&
           "jar".equals(object.getAttributeValue("extension")) &&
           isInFileFolderSystem(object)) return true;
        return false;
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        String entity = data[0].getModelEntity().getName();
        p = new Properties();
        String path = object.getPath();
        String rpath = XModelObjectLoaderUtil.getResourcePath(object);
        String fspath = path.substring(0, path.length() - rpath.length());
        XModelObject fso = object.getModel().getByPath(fspath);
        String location = fso.getAttributeValue("location") + rpath;
        p.setProperty("location", location);
        XModelObject c = h.mount(fso.getParent(), p, entity);
        c.setModified(false);
        FindObjectHelper.findModelObject(c, 0);
    }

    private boolean isInFileFolderSystem(XModelObject o) {
        while(o != null && o.getFileType() != XFileObject.SYSTEM) o = o.getParent();
        return o != null && o.getModelEntity().getName().equals("FileSystemFolder");
    }

}

