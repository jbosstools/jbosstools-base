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
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.util.*;

public class NewProjectHandler extends DefaultCreateHandler {

    public NewProjectHandler() {}

    public void executeHandler(XModelObject object, Properties prop) throws Exception {
        if(!isEnabled(object)) return;
        Properties p = extractProperties(data[0]);
        String dir = Paths.expand(p.getProperty("folder"), object.getModel().getProperties());
        String name = p.getProperty("name");
        XModel m = object.getModel();
        if(!save(m, action)) return;
        File f = (!"no".equals(p.getProperty("allocate new folder")))
                 ? createInNewFolder(m, dir, name)
                 : createNamedProject(m, dir, name);
        setGlobalJavaHome();
        m.load();
        notify(m, f);
    }

    private File createInNewFolder(XModel m, String dir, String name) {
        File d = new File(dir);
        d = new File(d, name);
        if(d.isDirectory())
            throw new RuntimeException("Cannot write new project over an old one.\nRemove directory " + d.getAbsolutePath() + " first.");
        File f = new File(d, "workspace.pex");
        doCreateProject(m, f, null);
        return f;
    }

    private File createNamedProject(XModel m, String dir, String name) {
        File d = new File(dir);
        File f = new File(d, name + "-" + "workspace.pex");
        doCreateProject(m, f, name);
        return f;
    }

    private void doCreateProject(XModel m, File f, String name) {
        try {
            if(f.exists()) throw new Exception();
            f.getParentFile().mkdirs();
            f.createNewFile();
        } catch (Exception e) {
            throw new RuntimeException(getMessageById("INVALID_TEMPLATE_LOCATION"));
        }
        XModelConstants.setWorkspace(m, f.getParent().replace('\\', '/'));
        XModelConstants.setWorkspaceName(m, name);
    }

    public static boolean save(XModel model, XAction action) {
        if(!model.getRoot().isModified()) return true;
        String h = action.getDisplayName();
        if(h.endsWith("...")) h = h.substring(0, h.length() - 3);
        int i = model.getService().showDialog(h,
                getMessageById("SAVE_REQUEST"),
                new String[]{"Yes", "No", "Cancel"}, null, 0);
        if(i == 2 || i < 0) return false;
        if(i == 0) model.save();
        return true;
    }

    public static void notify(XModel model, File f) {
        OpenedProjectsImpl p = (OpenedProjectsImpl)model.getRoot().getChildByPath("Workspaces");
        p.last(f.getAbsolutePath());
        model.getRoot("Options").setModified(true);
        model.saveOptions();
        model.save();
    }

    private void setGlobalJavaHome() {
        if(System.getProperty("global.JAVA_HOME") != null) return;
        String s = System.getProperty("java.home");
        if(s.endsWith("jre")) s = s.substring(0, s.length() - 4);
        System.setProperty("global.JAVA_HOME", s);
    }

}

