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
import org.jboss.tools.common.model.event.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;

//TODO check if this class is still needed

public abstract class ViewAgentHandler extends AbstractHandler {
    static String wizardname = "XXX.XActionSilentAgentWizard";
    static SpecialWizard wizard = null;

    static SpecialWizard wizard() {
        if(wizard == null) wizard = SpecialWizardFactory.createSpecialWizard(wizardname);
        return wizard;
    }

    public ViewAgentHandler() {}

    public boolean isEnabled(XModelObject object) {
        return (wizard() != null && object != null);
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        AppendTextEvent ev = createTextEvent(object);
        wizard.setObject(new Object[]{object, ev});
        wizard.execute();
    }

    protected abstract AppendTextEvent createTextEvent(XModelObject object);

}
