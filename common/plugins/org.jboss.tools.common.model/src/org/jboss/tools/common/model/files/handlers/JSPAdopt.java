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
package org.jboss.tools.common.model.files.handlers;

import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.action.impl.*;

public class JSPAdopt extends CompoundAdoptManager {
	/*
	 * see extension point org.jboss.tools.common.model.jspAdopt
    private static String[] MANAGERS = {"org.jboss.tools.jst.web.tld.model.handlers.JSPAdopt",
                                        "org.jboss.tools.struts.tiles.model.handlers.JSPAdopt",
                                        "org.jboss.tools.struts.model.handlers.JSPAdopt",
                                        "org.jboss.tools.jsf.model.handlers.JSPAdopt",
                                        "org.jboss.tools.jsf.ui.adopt.JSPAdopt",
                                        "org.jboss.tools.jst.web.model.handlers.JSPAdopt",
                                        };
    */
    private static XAdoptManager[] managers = null;

    public XAdoptManager[] getManagers() {
        if(managers == null) managers = loadManagers("org.jboss.tools.common.model.jspAdopt");
        return managers;
    }

}

