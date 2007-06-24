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
package org.jboss.tools.common.model.filesystems.impl.example;

import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.loaders.impl.DefaultEntityLoader;
import org.jboss.tools.common.model.filesystems.*;
import org.jboss.tools.common.model.filesystems.impl.*;

public class FileMainLoader extends DefaultEntityLoader {
//    private String MAIN_ENTITY = "XT_Main";
    private String EXT_ENTITY = "XT_External";
    private FileAuxiliary aux = new FileAuxiliary("efe", true);

    public FileMainLoader() {}

    public void load(XModelObject object) {
        super.load(object);
        String body = aux.read(object.getParent(), object);
        if(body == null) return;
        XModelObject ext = object.getChildren(EXT_ENTITY)[0];
        XModelObjectLoaderUtil.setTempBody(ext, body);
        XModelObjectLoaderUtil.getObjectLoader(ext).load(ext);
    }

    public boolean update(XModelObject object) {
        FileAnyImpl c = (FileAnyImpl)object.copy(0);
        XModelObject p = object.getParent();
        FolderLoader fl = (FolderLoader)p;
        c.setBodySource(fl.getBodySource(FileAnyImpl.toFileName(c)));
        c.setModified(false);
        p.removeChild(object);
        aux.getAuxiliaryFile(p, c, true);
        p.addChild(c);
        return true;
    }

    public boolean save(XModelObject object) {
        if(!object.isModified()) return true;
        XModelObject copy = object.copy();
        XModelObject ext = copy.getChildren(EXT_ENTITY)[0];
        ext.removeFromParent();
        if(!super.save(copy)) return false;
        XModelObjectLoaderUtil.setTempBody(object, XModelObjectLoaderUtil.getTempBody(copy));
        ext.setModified(true);
        if(!XModelObjectLoaderUtil.getObjectLoader(ext).save(ext)) return false;
        if(!aux.write(object.getParent(), object, ext)) return false;
        return true;
    }

}

