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

import java.io.*;
import java.util.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.Paths;
import org.jboss.tools.common.model.impl.*;

public class OpenProjectHandler extends DefaultCreateHandler {

    public OpenProjectHandler() {}

    public void executeHandler(XModelObject object, Properties prop) throws Exception {
        if(!isEnabled(object)) return;
        XEntityData[] es = (XEntityData[])data;
        Properties p = extractProperties(es[0]);
        String project = p.getProperty("redhat project");
        project = Paths.expand(project, object.getModel().getProperties()).replace('\\', '/');
        if(project.lastIndexOf(':') < 2) {
            File f = new File(project);
            if(!f.exists() || !f.isFile() || !project.endsWith(".pex"))
              throw new RuntimeException(getMessageById("INVALID_TEMPLATE_LOCATION"));
        }
        XModel m = object.getModel();
        if(!NewProjectHandler.save(m, action)) return;

        int q = project.lastIndexOf('/');
        String ws = project.substring(0, q);
        XModelConstants.setWorkspace(m, ws);
/*        
        String n = project.substring(q + 1);
        q = n.lastIndexOf('-');
        String wsn = (q < 0) ? null : n.substring(0, q);
        XModelConstants.setWorkspaceName(m, wsn);
*/
		XModelConstants.setWorkspaceName(m, getProjectNameFromPex(project.substring(q + 1)));
        m.load();
        notify(m, project);
    }
    
    public static String getProjectNameFromPex(String pexFileName)
    {
		int q = pexFileName.lastIndexOf('-');
		return (q < 0) ? null : pexFileName.substring(0, q);
    }

    public static void notify(XModel model, String last) {
        OpenedProjectsImpl p = (OpenedProjectsImpl)model.getRoot().getChildByPath("Workspaces");
        p.last(last);
        model.getRoot("Options").setModified(true);
        model.saveOptions();
        model.save();
    }

    public XEntityData[] getEntityData(XModelObject object) {
        super.getEntityData(object);
        XAttributeData a = data[0].getAttributeData()[0];
        String s = XModelConstants.getWorkspace(object.getModel());
        if(s == null || s.length() == 0) return data;
        File f = new File(s);
        s = f.getParent().replace('\\', '/');
        a.setValue(s);
        return data;
    }

}

