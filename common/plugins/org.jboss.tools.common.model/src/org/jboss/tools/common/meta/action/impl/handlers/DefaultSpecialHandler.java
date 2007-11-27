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

import java.util.Properties;

import org.eclipse.core.runtime.Status;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;

public class DefaultSpecialHandler extends AbstractHandler {
    private boolean validated = false;
    private SpecialWizard wizard = null;
    private SpecialWizardSupport support = null;

    public DefaultSpecialHandler() {}

    public static SpecialWizardSupport createSpecialWizardSupport(String id) {
        try {
            return (SpecialWizardSupport)ModelFeatureFactory.getInstance().createFeatureInstance(id);
        } catch (Exception e) {
			ModelPlugin.getDefault().getLog().log(new Status(Status.ERROR, ModelPlugin.PLUGIN_ID, Status.OK, "Model warning: Cannot load special wizard support " + id + ".",e));
            return null;
        }
    }

    private void validate() {
        if(validated) return;
        validated = true;
        wizard = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.ui.wizards.special.DefaultSpecialWizard");
        support = createSpecialWizardSupport(action.getProperty("support"));
        support.setAction(action);
    }

    public boolean isEnabled(XModelObject object) {
        validate();
        return object != null && wizard != null && support != null && support.isEnabled(object);
    }

    public boolean isEnabled(XModelObject object, XModelObject[] objects) {
        if(object != null && (objects == null || objects.length == 1)) return isEnabled(object);
        return false;
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        support.setActionData(action, data, object, p);
        if(support.isFinished()) return;
        wizard.setObject(new Object[]{support});
        wizard.execute();
    }

}

