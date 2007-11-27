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
package org.jboss.tools.common.model.engines.impl;

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.meta.action.impl.*;

public class StopProcessHandler extends AbstractHandler {

    public StopProcessHandler() {}

    public boolean isEnabled(XModelObject object) {
        if(object == null) return false;
        return (object != null && XProcessStorage.getDefaultStorage().getRunningInstances(object.getPath()) != null);
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        String path = object.getPath();
        XProcessStorage s = XProcessStorage.getDefaultStorage();
        HashMap<Long,XProcess> map = s.getRunningInstances(path);
        Long[] ls = map.keySet().toArray(new Long[0]);
        for (int i = 0; i < ls.length; i++) {
            s.stopInstance(path, ls[i].longValue());
            object.getModel().getOut().println("Process executing " + FindObjectHelper.makeRef(object) + " (" + new Date(ls[i].longValue()) + ") stopped.");
        }
    }

}
