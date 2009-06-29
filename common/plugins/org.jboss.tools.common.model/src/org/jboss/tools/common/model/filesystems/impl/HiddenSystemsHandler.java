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
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.model.util.*;

public class HiddenSystemsHandler extends AbstractHandler {
//    SpecialWizard wizard = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.ui.wizards.query.list.HiddenFileSystemsWizard");

    public HiddenSystemsHandler() {}

    public boolean isEnabled(XModelObject object) {
        return (/*wizard != null &&*/ object != null && object.isObjectEditable());
    }

    public void executeHandler(XModelObject object, Properties p) throws XModelException {
        if(!isEnabled(object)) return;
		SpecialWizard wizard = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.ui.wizards.query.list.HiddenFileSystemsWizard"); //$NON-NLS-1$
        XModelObject[] fs = object.getChildren();
        String[][] vs = new String[fs.length][];
        for (int i = 0; i < vs.length; i++) {
            String name = fs[i].getAttributeValue(XModelObjectConstants.ATTR_NAME);
            Properties fsp = XModelObjectUtil.toProperties(fs[i]);
            String hidden = "" + fsp.getProperty("hidden", XModelObjectConstants.NO); //$NON-NLS-1$ //$NON-NLS-2$
            String jar = "" + "FileSystemJar".equals(fs[i].getModelEntity().getName()); //$NON-NLS-1$ //$NON-NLS-2$
            vs[i] = new String[]{name, hidden, jar};
        }
        if(p == null) p = new Properties();
        p.put("data", vs); //$NON-NLS-1$
        p.setProperty("help", "FileSystems_ShowHide"); //$NON-NLS-1$ //$NON-NLS-2$
        wizard.setObject(p);
        if(wizard.execute() != 0) return;
        boolean ch = false;
        for (int i = 0; i < vs.length; i++) {
            Properties fsp = XModelObjectUtil.toProperties(fs[i]);
            String hidden = "" + fsp.getProperty("hidden", XModelObjectConstants.NO); //$NON-NLS-1$ //$NON-NLS-2$
            if(hidden.equals(vs[i][1])) continue;
            fsp.setProperty("hidden", vs[i][1]); //$NON-NLS-1$
            fs[i].setAttributeValue("info", XModelObjectUtil.toString(fsp)); //$NON-NLS-1$
            fs[i].setModified(true);
            ch = true;
        }
        if(ch) {
            XModelImpl m = (XModelImpl)object.getModel();
            m.fireStructureChanged(object);
        }
    }

}
