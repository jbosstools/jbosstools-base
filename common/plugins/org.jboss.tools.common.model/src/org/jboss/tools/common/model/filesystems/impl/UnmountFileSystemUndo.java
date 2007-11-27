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
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.undo.*;

public class UnmountFileSystemUndo extends XUndoableImpl {
    protected XModel model = null;
    protected String entity;
    protected String pathpart;
    protected Properties p = new Properties();
    protected String[] pns = {"name", "location", "slave model", "file system"};

    public UnmountFileSystemUndo(XModelObject fs) {
        model = fs.getModel();
        pathpart = fs.getPathPart();
        entity = fs.getModelEntity().getName();
        for (int i = 0; i < pns.length; i++) {
          if(fs.getModelEntity().getAttribute(pns[i]) != null)
            p.setProperty(pns[i], fs.getAttributeValue(pns[i]));
        }
        String nm = (p.getProperty(pns[1]) != null) ? p.getProperty(pns[1]) :
                    "Engines/remote model/ " + p.getProperty(pns[2]) +
                    ":FileSystems/" + p.getProperty(pns[3]);  
        description = "Unmount file system " + nm;
    }

    protected void doUndo() {
        XModelObject o = model.createModelObject(entity, p);
        model.getByPath("FileSystems").addChild(o);
		MountFileSystemHandler.updateClassPath(o);
    }

    protected void doRedo() {
        XModelObject o = model.getByPath("FileSystems/" + pathpart);
        if(o != null) o.removeFromParent();
		MountFileSystemHandler.updateClassPath(o);
    }

    protected String getActionIcon() {
        return "images/actions/delete.gif";
    }

}

