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
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;

public class SaveAllHandler extends AbstractHandler {
	SpecialWizard saveEditors = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.ui.objecteditor.SaveEditorsSpecialWizard");
    public SaveAllHandler() {}

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(object == null) return;
        if(saveEditors != null) saveEditors.execute();
        object.getModel().save();
        object.getModel().getUndoManager().reset();

        object.getModel().getChangeManager().fire();
    }

    public boolean isEnabled(XModelObject object) {
        return object != null && object.getModel().getRoot().isModified();
    }

} 