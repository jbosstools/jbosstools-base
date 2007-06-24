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
package org.jboss.tools.common.meta.help;

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;

public class HelpHandler extends AbstractHandler {

    public HelpHandler() {}

    public boolean isEnabled(XModelObject object) {
        return (object != null);
    }

    public void executeHandler(XModelObject object, Properties prop) throws Exception {
        if(!isEnabled(object)) return;
        help(object.getModel(), object.getModelEntity().getName());
    }

    protected void help(XModel model, String key) throws Exception {
		HelpUtil.helpEclipse(model, key);
    }
    
}
