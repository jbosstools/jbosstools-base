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
package org.jboss.tools.common.meta.action.impl.handlers;

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.help.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;

public class PropertiesHandler extends AbstractHandler {
    private static String wizardname = "org.jboss.tools.common.model.ui.objecteditor.PropertiesWizard";

    public PropertiesHandler() {}

    public boolean isEnabled(XModelObject object) {
        return (object != null);
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        String viewMode = action.getProperty("viewMode");
        p = HelpUtil.createKey(object, action, p);
        if(viewMode != null) p.put("viewMode", viewMode);
		SpecialWizard wizard = SpecialWizardFactory.createSpecialWizard(wizardname);
        wizard.setObject(new Object[]{object, p});
        wizard.execute();
    }

    public boolean isEnabled(XModelObject object, XModelObject[] objects) {
        return false;
    }

}
 