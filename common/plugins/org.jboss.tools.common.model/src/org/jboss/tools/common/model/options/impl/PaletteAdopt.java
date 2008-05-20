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
package org.jboss.tools.common.model.options.impl;

import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.impl.*;

public class PaletteAdopt extends CompoundAdoptManager {
	/*
	 * see extension point org.jboss.tools.common.model.paletteAdopt
    private static String[] MANAGERS = {"org.jboss.tools.jst.web.tld.model.handlers.PaletteAdopt"};
    */
    private static XAdoptManager[] managers = null;

    protected String getExtensionPoint() {
    	return "org.jboss.tools.common.model.paletteAdopt";
    }

    public XAdoptManager[] getManagers() {
        if(managers == null) managers = loadManagers("org.jboss.tools.common.model.paletteAdopt");
        return managers;
    }

}

