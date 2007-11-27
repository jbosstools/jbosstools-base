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

import org.jboss.tools.common.meta.action.impl.handlers.DefaultRemoveHandler;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.loaders.*;

public class UnmountFileSystemHandler extends DefaultRemoveHandler {

    public UnmountFileSystemHandler() {}

    public void executeHandler(XModelObject object, java.util.Properties p) throws Exception {
        if(!isEnabled(object)) return;
        if(!handleModified(object)) return;
        XModelObject parent = object.getParent();
        object.removeFromParent();
        object.getModel().getUndoManager().addUndoable(new UnmountFileSystemUndo(object));
        parent.setModified(true);
        if(parent instanceof FileSystemsImpl) {
            ((FileSystemsImpl)parent).updateOverlapped();
        }
		MountFileSystemHandler.updateClassPath(object);
    }

    public boolean isEnabled(XModelObject object) {
    	// We do not need this action in Eclipse
    	return false;
//        return (object != null && object.isActive());
    }

    public boolean getSignificantFlag(XModelObject object) {
        return object != null && !object.isModified();
    }

    private boolean handleModified(XModelObject object) {
        if(!object.isModified()) return true;
        ServiceDialog d = object.getModel().getService();
        String mes = "File system " + object.get("NAME") + " was modified.\n" +
                     "Do you want to save changes?";
        int i = d.showDialog(action.getDisplayName(), mes, new String[]{"Yes", "No", "Cancel"}, null, ServiceDialog.QUESTION);
        if(i == 1) return true;
        if(i != 0) return false;
        XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(object);
        return (loader == null || loader.save(object));
    }
    
}
