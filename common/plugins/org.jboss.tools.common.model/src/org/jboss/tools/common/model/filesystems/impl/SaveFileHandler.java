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

import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;

public class SaveFileHandler extends AbstractHandler {
	static SpecialWizard wizard = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.ui.objecteditor.SaveEditorSpecialWizard");
    public SaveFileHandler() {}

    public boolean isEnabled(XModelObject object) {
        return (object != null && object.isModified() && object.isActive() &&
                (object.getParent() instanceof FolderImpl));
    }

    public void executeHandler(XModelObject object, java.util.Properties p) throws Exception {
        if(!isEnabled(object)) return;
        int q = -1;
        if(wizard != null) {
        	wizard.setObject(object);
        	q = wizard.execute();
        }
        if(q != 0) {
        	if(object.getParent() instanceof FolderImpl) {
        		FolderImpl folder = (FolderImpl)object.getParent();
        		folder.saveChild(object);
        	} else {
        		object.setModified(false);
        		XModelObjectLoaderUtil.updateModifiedOnSave(object);
        	}
		}
    }

}
