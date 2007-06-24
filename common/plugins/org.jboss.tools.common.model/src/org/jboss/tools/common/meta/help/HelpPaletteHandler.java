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

public class HelpPaletteHandler extends HelpHandler {

    public HelpPaletteHandler() {}

    public void executeHandler(XModelObject object, Properties prop) throws Exception {
        if(!isEnabled(object)) return;
        XModelObject tab = getTabObject(object);
        String key = (tab == null) ? null : tab.getModelEntity().getName() + "_" + tab.getPathPart().replace(' ', '_');
        if(key == null || !HelpUtil.hasHelp(key)) {
            super.executeHandler(object, prop);
        } else {
            HelpUtil.callHelp(object.getModel(), key);
        }
    }

    private XModelObject getTabObject(XModelObject object) {
        XModelObject tab = object;
        while(tab != null && !tab.getModelEntity().getName().startsWith("SharablePageTab"))
          tab = tab.getParent();
        return tab;
    }
    
}
